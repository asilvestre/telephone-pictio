{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import GHC.Generics
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Vector ((!?))

import qualified Data.ByteString as BS
import qualified Data.HashMap as HM
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

data GameStep = Picture BS.ByteString | Phrase T.Text deriving (Show)

instance ToJSON GameStep where
  toJSON (Picture d) = object ["type" .= ("picture" :: T.Text), "data" .= T.decodeUtf8 d]
  toJSON (Phrase t) = object ["type" .= ("phrase" :: T.Text), "data" .= t]

instance FromJSON GameStep where
  parseJSON (Object v) = do
    t <- v .: "type"
    tv <- v .: "data"
    parseStep t tv
    where
      parseStep (String t) (String v)
        | t == "picture" = return $ Picture (T.encodeUtf8 v)
        | t == "phrase" = return $ Phrase v
        | otherwise =  mzero
      parseStep _ _ = mzero
  parseJSON _ = mzero

data Game = Game (V.Vector GameStep) deriving (Show)

instance ToJSON Game where
  toJSON (Game ss) = object ["steps" .= V.toList ss]

newtype IntT u = IntT { runIntT :: Int } deriving (Eq, Show, Ord, Num, Generic, Hashable, FromHttpApiData, Bounded,
  Integral, Enum, Real)

instance FromJSON (IntT u) where
  parseJSON (Number n) = return (fromMaybe 0 $ toBoundedInteger n)
  parseJSON _ = mzero

instance ToJSON (IntT u) where
  toJSON (IntT n) = toJSON n

data GameIdT
type GameId = IntT GameIdT

data StepIdT
type StepId = IntT StepIdT

data GamePath = GamePath {gameId :: GameId, stepId :: StepId} deriving (Eq, Ord, Show, Generic)

instance FromJSON GamePath
instance ToJSON GamePath

type GameDB = HM.HashMap GameId Game

type PhonePictioAPI =
  "games" :> Get '[JSON] [GameId]
  :<|> "game" :> Capture "gameid" GameId :> Get '[JSON] Game
  :<|> "game" :> ReqBody '[JSON] GameStep :> Post '[JSON] GamePath
  :<|> "gamestep" :> Capture "gameid" GameId :> Capture "stepid" StepId :> Get '[JSON] GameStep
  :<|> "gamestep" :> Capture "gameid" GameId :> Capture "stepid" StepId :> ReqBody '[JSON] GameStep :> Post '[JSON] GamePath

someFunc :: IO ()
someFunc = do
  tvar <- atomically $ newTVar HM.empty
  run 8081 (app tvar)


gameAPI :: Proxy PhonePictioAPI
gameAPI = Proxy

app :: TVar GameDB -> Application
app tvar = serve gameAPI (server tvar)

server :: TVar GameDB -> Server PhonePictioAPI
server st = getGames st :<|> getGame st :<|> postGame st :<|> getGameStep st :<|> postGameStep st

getGames :: TVar GameDB -> Handler [GameId]
getGames tvar = do
  db <- liftIO $ atomically $ readTVar tvar
  return $ HM.keys db

getGame :: TVar GameDB -> GameId -> Handler Game
getGame tvar gid = do
  db <- liftIO $ atomically $ readTVar tvar
  case HM.lookup gid db of
    Nothing -> throwError err404
    Just g -> return g

postGame :: TVar GameDB -> GameStep -> Handler GamePath
postGame tvar s = liftIO $ atomically $ do
  db <- readTVar tvar
  let newId = IntT $ HM.size db
  writeTVar tvar $ HM.insert newId (Game $ V.singleton s) db
  return $ GamePath newId 0

getGameStep :: TVar GameDB -> GameId -> StepId -> Handler GameStep
getGameStep tvar gid sid = do
  db <- liftIO $ atomically $ readTVar tvar
  case HM.lookup gid db >>= (\(Game ss) -> ss !? runIntT sid) of
    Nothing -> throwError err404 { errBody = "Step not found" }
    Just s -> return s

postGameStep :: TVar GameDB -> GameId -> StepId -> GameStep -> Handler GamePath
postGameStep tvar gid sid s = do
  mr <- liftIO $ atomically $ do
    db <- readTVar tvar
    let idx = runIntT sid
    let mg = HM.lookup gid db
    case mg >>= (\(Game ss) -> ss !? idx) of
      Nothing -> return $ Left "Step not found"
      Just os -> do
        let Just (Game ss) = mg
        let nextStep = IntT $ V.length ss
        let (ngid, rss) = if nextStep > sid + 1 then (IntT $ HM.size db, V.slice 0 (idx + 1) ss) else (gid, ss)
        let nss = rss `V.snoc` s
        if correctStep os s then (do
                            writeTVar tvar $ HM.insert ngid (Game nss) db
                            return $ Right (GamePath ngid (IntT $ V.length nss)))
                            else return $ Left "Incompatible step"
  case mr of
    Left err -> throwError err404 { errBody = err }
    Right r -> return r
  where
    correctStep (Picture _) (Phrase _) = True
    correctStep (Phrase _) (Picture _) = True
    correctStep _ _ = False
