{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


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
import Servant.HTML.Blaze
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Vector ((!?))

import qualified Data.ByteString as BS
import qualified Data.HashMap as HM
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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

newtype IntT u = IntT Int deriving (Eq, Show, Ord, Num, Generic, Hashable, FromHttpApiData, Bounded,
  Integral, Enum, Real, FromJSON, ToJSON)

runIntT :: IntT u -> Int
runIntT (IntT n) = n

data GameIdT
type GameId = IntT GameIdT

newtype GameList = GameList [GameId] deriving(Eq, Show, FromJSON, ToJSON)

data StepIdT
type StepId = IntT StepIdT

data StepPath = StepPath {gameId :: GameId, stepId :: StepId} deriving (Eq, Ord, Show, Generic)

instance FromJSON StepPath
instance ToJSON StepPath

type GameDB = HM.HashMap GameId Game

instance B.ToMarkup GameList where
  toMarkup (GameList gs) = do
    H.head $ H.title "TelephonePictio - Game List"
    H.body $ do
      H.h1 "Game list"
      H.ol $ forM_ gs (H.li . B.toMarkup)
      H.a H.! A.href "/game/" $ "create a new one!"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

instance B.ToMarkup GameId where
  toMarkup (IntT n) = H.a H.! A.href (B.toValue $ "/game/" <> tshow n) $ (H.toHtml $ "Game " <> tshow n)

instance B.ToMarkup Game where
  toMarkup (Game ss) = do
    H.head $ H.title "TelephonePictio - Game"
    H.body $ do
      H.h1 "Game"
      forM_ ss B.toMarkup

instance B.ToMarkup GameStep where
  toMarkup (Picture d) = H.img H.! A.src (B.toValue $ "data:image/png;base64," <> tshow d)
  toMarkup (Phrase d) = H.h2 $ H.toHtml d

type PhonePictioAPI =
  "games" :> Get '[JSON, HTML] GameList
  :<|> "game" :> Capture "gameid" GameId :> Get '[JSON, HTML] Game
  :<|> "game" :> ReqBody '[JSON] GameStep :> Post '[JSON] StepPath
  :<|> "game" :> Capture "gameid" GameId :> "step" :> Capture "stepid" StepId :> Get '[JSON, HTML] GameStep
  :<|> "game" :> Capture "gameid" GameId :> "step" :> Capture "stepid" StepId :> ReqBody '[JSON] GameStep :> Post '[JSON] StepPath

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

getGames :: TVar GameDB -> Handler GameList
getGames tvar = do
  db <- liftIO $ atomically $ readTVar tvar
  return $ GameList (HM.keys db)

getGame :: TVar GameDB -> GameId -> Handler Game
getGame tvar gid = do
  db <- liftIO $ atomically $ readTVar tvar
  case HM.lookup gid db of
    Nothing -> throwError err404 { errBody = "Game not found" }
    Just g -> return g

postGame :: TVar GameDB -> GameStep -> Handler StepPath
postGame tvar s = liftIO $ atomically $ do
  db <- readTVar tvar
  let newId = IntT $ HM.size db
  writeTVar tvar $ HM.insert newId (Game $ V.singleton s) db
  return $ StepPath newId 0

getGameStep :: TVar GameDB -> GameId -> StepId -> Handler GameStep
getGameStep tvar gid sid = do
  db <- liftIO $ atomically $ readTVar tvar
  case HM.lookup gid db >>= (\(Game ss) -> ss !? runIntT sid) of
    Nothing -> throwError err404 { errBody = "Step not found" }
    Just s -> return s

postGameStep :: TVar GameDB -> GameId -> StepId -> GameStep -> Handler StepPath
postGameStep tvar gid sid s = do
  mr <- liftIO $ atomically $ do
    db <- readTVar tvar
    let idx = runIntT sid
    let mg = HM.lookup gid db
    case mg >>= (\(Game ss) -> ss !? idx) of
      Nothing -> return $ Left (err404 { errBody = "Step not found"})
      Just os -> do
        let Just (Game ss) = mg
        let nextStep = IntT $ V.length ss
        let (ngid, rss) = if nextStep > sid + 1 then (IntT $ HM.size db, V.slice 0 (idx + 1) ss) else (gid, ss)
        let nss = rss `V.snoc` s
        if correctStep os s then (do
                            writeTVar tvar $ HM.insert ngid (Game nss) db
                            return $ Right (StepPath ngid (IntT $ V.length nss)))
                            else return $ Left (err400 { errBody = "Incompatible step" })
  case mr of
    Left err -> throwError err
    Right r -> return r
  where
    correctStep (Picture _) (Phrase _) = True
    correctStep (Phrase _) (Picture _) = True
    correctStep _ _ = False
