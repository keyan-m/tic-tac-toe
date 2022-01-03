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
-- import Network.WebSockets
--   ( PendingConnection
--   , Connection
--   , forkPingThread
--   , sendTextData
--   , receiveDataMessage
--   )
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
  ( WebSocketPending
  )
import RandomCode
import Game (Game (..), ElmGame (..))
import qualified Game
import Player (Player (..), Players (..), ElmPlayer (..), ElmPlayers (..))
import qualified Player
import qualified Join
import Vessel (Vessel)
import qualified Vessel
-- }}}



-- API
-- {{{
type Api
  ---- The initial request from the X player ----------------------- v
     = "new"   :> ReqBody '[JSON] ElmPlayer
               :> Post    '[JSON] ElmGame
  ------------------------------------------------------------------ ^
  --
  ---- The initial request from the O player ----------------------- v
  :<|> "join"  :> Capture "gameCode" String
               :> ReqBody '[JSON] ElmPlayer
               :> Post    '[JSON] Join.ElmResult
  ------------------------------------------------------------------ ^
  --
  ---- Request to close a game that is waiting for another player -- v
  :<|> "close" :> ReqBody '[JSON] String
               :> Post    '[JSON] ()
  ------------------------------------------------------------------ ^
  --
  ---- Endpoint for WebSocket -------------------------------------- v
  :<|> "play"  :> Capture "gameCode" String
               :> WebSocketPending
  ------------------------------------------------------------------ ^
  --
  ---- Endpoint for serving static files --------------------------- v
  :<|> Raw
  ------------------------------------------------------------------ ^

api :: Proxy Api
api = Proxy

type ApiHandler = ReaderT AppState Servant.Handler
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



-- HANDLERS
-- {{{
newGameHandler :: ElmPlayer -> ApiHandler ElmGame
newGameHandler elmPlayer = do
  -- {{{
  appState <- ask -- :: ReaderT r m r
  allGames <- getGames appState
  newGameCode <- uniqueRandom6Chars (map Game.getCode allGames)
  let newGame = Game.new newGameCode elmPlayer
  atomically
    ( writeTVar
        (games appState)
        (newGame : allGames)
    )
    & fmap (const $ Game.toElm newGame)
  -- }}}


joinGameFromPool :: String -> ElmPlayer -> [Game] -> (Join.ElmResult, [Game])
joinGameFromPool targetCode elmPlayer currPool =
  -- {{{
  let
    go gs soFar =
      case gs of
        [] ->
          (Join.elmFailedInvalidCode, soFar)
        g : restOfGames ->
          let
            joinResult = Join.attempt targetCode elmPlayer g
          in
          case joinResult of
            Join.Failed _ ->
              go restOfGames (soFar ++ [g])
            Join.Successful joinedGame ->
              ( Join.toElm joinResult
              , soFar ++ (joinedGame : restOfGames)
              )
  in
  go currPool []
  -- }}}


joinGameHandler :: String -> ElmPlayer -> ApiHandler Join.ElmResult
joinGameHandler targetCode elmPlayer = do
  -- {{{
  appState <- ask
  allGames <- getGames appState
  let (joinResult, updatedPool) =
        joinGameFromPool targetCode elmPlayer allGames
  atomically
    ( writeTVar (games appState) updatedPool
    ) & fmap (const joinResult)
  -- }}}


closeGameHandler :: String -> ApiHandler ()
closeGameHandler code = do
  -- {{{
  appState <- ask
  allGames <- getGames appState
  let newGames = filter (\g -> (Game.getCode g) /= code) allGames
  atomically $ writeTVar (games appState) newGames
  -- }}}


updatePoolFrom :: WS.Connection -> String -> Vessel -> [Game] -> (Vessel, [Game])
updatePoolFrom conn targetCode givenVes pool =
  -- {{{
  let
    go :: (Game -> Game) -> [Game] -> [Game] -> (Vessel, [Game])
    go updateFn remainingGames soFar =
      -- {{{
      case remainingGames of
        [] ->
          -- {{{
          (Vessel.GameNotFound, soFar)
          -- }}}
        g : gs ->
          -- {{{
          let
            currCode = Game.getCode g
          in
          if currCode == targetCode then
            ( Vessel.Empty
            , soFar ++ (updateFn g : gs)
            )
          else
            go updateFn gs (soFar ++ [g])
          -- }}}
      -- }}}
  in
  case givenVes of
    Vessel.Empty ->
      -- {{{
      (Vessel.Empty, pool)
      -- }}}
    Vessel.XPlayerRegistration (ElmPlayer playerTag) ->
      -- {{{
      go
        ( \(Game info players) ->
              Game info $ players
                { Player.xPlayer = Just $ Player playerTag (Just conn)
                }
        )
        pool
        []
      -- }}}
    Vessel.OPlayerJoinRequest  (ElmPlayer playerTag) ->
      -- {{{
      go
        ( \(Game info players) ->
              Game info $ players
                { Player.oPlayer = Just $ Player playerTag (Just conn)
                }
        )
        pool
        []
      -- }}}
  -- }}}


wsHandler :: String -> WS.PendingConnection -> ApiHandler ()
wsHandler gameCode pendingConn = do
  -- {{{
  appState <- ask
  allGames <- getGames appState
  --
  conn           <- liftIO $ WS.acceptRequest pendingConn
  receivedVessel <- liftIO $ WS.receiveData conn
  --
  let (responseVessel, newPool) =
        updatePoolFrom conn gameCode receivedVessel allGames
  liftIO $ WS.sendTextData conn responseVessel
  atomically $ writeTVar (games appState) newPool
  -- liftIO $ sendTextData conn bsFromClient
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
