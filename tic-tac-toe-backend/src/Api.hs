-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
-- }}}



-- MODULE
-- {{{
module Api
    ( startApp
    , app
    , initApp
    ) where
-- }}}



-- IMPORTS
-- {{{
-- import Debug.Trace (trace)
import ClassyPrelude
import Data.Function ((&))
import GHC.Generics (Generic)

import Control.Concurrent (threadDelay)
-- import Control.Concurrent.STM.TVar (TVar, newTVar, readTVarIO, writeTVar)
-- import Control.Monad.STM (atomically)

import qualified Data.Text as T
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.WebSockets as WS
import Network.WebSockets
  ( Connection
  , forkPingThread
  , sendTextData
  , receiveDataMessage
  )
import Servant
import Servant.Server.StaticFiles (serveDirectoryWith)
import qualified WaiAppStatic.Types as WaiStatic
import WaiAppStatic.Storage.Filesystem
  ( defaultWebAppSettings
  )
import Lucid.Base
import Lucid.Html5
import Servant.HTML.Lucid
import Servant.API.WebSocket
  ( WebSocket
  )
import RandomCode
import ConnectionRequest
import Game
  ( Game (..)
  , MaybeGame (..)
  , getGameCode
  , startAGame
  , Msg (..)
  , update
  )
import qualified Game
import Vessel
-- }}}



-- UTILS
-- {{{
data AppState = AppState
  { games :: TVar [Game]
  } deriving (Generic)

initApp :: IO AppState
initApp = do
  oGs <- atomically $ newTVar []
  return $ AppState
    { games = oGs
    }

getGames :: AppState -> ApiHandler [Game]
getGames appState = do
  -- {{{
  oGs <- readTVarIO $ games appState
  return oGs
  -- }}}
-- }}}



-- API
-- {{{
type Api
  --   This is the initial request from the X player.
     = "new"   :> ReqBody '[JSON] String       :> Post '[JSON] String
  :<|> "join"  :> ReqBody '[JSON] JoinGameInfo :> Post '[JSON] MaybeGame
  :<|> "close" :> ReqBody '[JSON] String       :> Post '[JSON] ()
  :<|> "play"  :> Capture "gameCode"           :> WebSocket
  :<|> Raw

api :: Proxy Api
api = Proxy

type ApiHandler = ReaderT AppState Servant.Handler
-- }}}



-- HANDLERS
-- {{{
newGameHandler :: String -> ApiHandler String
newGameHandler xPTag = do
  -- {{{
  appState <- ask -- :: ReaderT r m r
  allGames <- getGames appState
  newGameCode <- uniqueRandom6Chars (map getGameCode allGames)
  atomically
    ( writeTVar (games appState) (WaitingForO newGameCode xP : allGames)
    ) & fmap (const newGameCode)
  -- }}}


joinGame :: [Game] -> ConnectionRequest -> ([Game], MaybeGame)
joinGame currPool req =
  -- {{{
  let
    go soFar gs =
      case gs of
        [] ->
          (soFar, MaybeGame Nothing)
        g : rest ->
          case Game.join g req of
            Nothing ->
              go (soFar ++ [g]) rest
            Just joinedGame ->
              ( soFar ++ (joinedGame : rest)
              , MaybeGame $ Just joinedGame
              )
  in
  go [] currPool
  -- }}}


joinGameHandler :: ConnectionRequest -> ApiHandler MaybeGame
joinGameHandler req = do
  -- {{{
  appState <- ask
  allGames <- getGames appState
  let (updatedGames, mNewGame) = joinGame allGames req
  atomically
    ( writeTVar (games appState) updatedGames
    ) & fmap (const mNewGame)
  -- }}}


closeGameHandler :: String -> ApiHandler ()
closeGameHandler code = do
  -- {{{
  appState <- ask
  allGames <- getGames appState
  let newGames = filter (\g -> (getGameCode g) /= code) allGames
  seq
    (trace ("ALL GAMES: " ++ show allGames) ())
    (atomically $ writeTVar (games appState) newGames)
  -- }}}


wsHandler :: String -> Connection -> ApiHandler ()
wsHandler gameCode conn = do
  -- {{{
  receivedText <- liftIO $ receiveDataMessage conn
  let bsFromClient =
    case receivedText of
      WS.Text byteStr _ -> byteStr
      WS.Binary byteStr -> byteStr
  -- (putStrLn $ T.pack $ show bsFromClient) >> (liftIO $ threadDelay 100000)
  case (decode bsFromClient :: Maybe Vessel) of
    Nothing ->
      return ()
    Just Vessel.OPlayerJoined ->
  liftIO $ sendTextData conn bsFromClient
  -- liftIO . forM_ [1..] $ \i -> do
  --   forkPingThread conn 10
  --   sendTextData conn (T.pack $ show (i :: Int)) >> threadDelay 1000000
  -- }}}
-- }}}



-- SERVER
-- {{{
server :: ServerT Api ApiHandler
server =
  -- {{{
       newGameHandler
  :<|> joinGameHandler
  :<|> closeGameHandler
  :<|> wsHandler
  :<|> ( serveDirectoryWith $
           let
             s = defaultWebAppSettings "../tic-tac-toe-frontend/dist"
           in
           s {WaiStatic.ssIndices = [WaiStatic.unsafeToPiece "index.html"]}
       )
  -- }}}
-- }}}



-- APPLICATION
-- {{{
app :: AppState -> Application
app appState =
  serve api $ hoistServer api (\x -> runReaderT x appState) server

startApp :: IO ()
startApp = do
  initAppState <- initApp
  putStrLn "\n============================"
  putStrLn "Starting server on port 8080"
  putStrLn "============================\n"
  run 8080 (app initAppState)
-- }}}
