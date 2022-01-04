-- EXTENSIONS
-- {{{
{-# LANGUAGE DataKinds         #-}
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
  oGs <- newTVarIO []
  return $ AppState
    { games = oGs
    }

getGames :: AppState -> ApiHandler [Game]
getGames appState = do
  -- {{{
  readTVarIO $ games appState
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
  let newGames = filter (\g -> Game.getCode g /= code) allGames
  atomically $ writeTVar (games appState) newGames
  -- }}}


data WrappedVessel
  = TwoVessels
      { xPayload :: (WS.Connection, Vessel)
      , oPayload :: (WS.Connection, Vessel)
      }
  | OneVessel WS.Connection Vessel

updatePoolFrom :: WS.Connection
               -> String
               -> Vessel
               -> [Game]
               -> (WrappedVessel, [Game])
updatePoolFrom conn targetCode givenVes pool =
  -- {{{
  let
    go :: (Game -> (WrappedVessel, Game))
       -> [Game]
       -> [Game]
       -> (WrappedVessel, [Game])
    go updateFn remainingGames soFar =
      -- {{{
      case remainingGames of
        [] ->
          -- {{{
          (OneVessel conn Vessel.GameNotFound, soFar)
          -- }}}
        g : gs ->
          -- {{{
          let
            currCode = Game.getCode g
          in
          if currCode == targetCode then
            let
              (newV, newG) = updateFn g
            in
            ( newV
            , soFar ++ (newG : gs)
            )
          else
            go updateFn gs (soFar ++ [g])
          -- }}}
      -- }}}
  in
  case givenVes of
    Vessel.Empty ->
      -- {{{
      (OneVessel conn Vessel.Empty, pool)
      -- }}}
    Vessel.RegistrationRequest (ElmPlayer playerTag False) ->
      -- {{{
      go
        ( \g@(Game info players) ->
              let
                mXP = Player.xPlayer players
                mOP = Player.oPlayer players
              in
              case (mXP, mOP) of
                (Nothing, Nothing) ->
                  -- {{{
                  ( OneVessel conn Vessel.RegistrationSuccessful
                  , Game info $ players
                      { Player.xPlayer = Just $ Player playerTag (Just conn)
                      }
                  )
                  -- }}}
                (Just xP, Nothing) ->
                  -- {{{
                  let
                    newGame =
                      Game info $ players
                        { Player.oPlayer =
                            Just $ Player playerTag (Just conn)
                        }
                  in
                  ( case Player.conn xP of
                      Just xConn ->
                        -- {{{
                        TwoVessels
                          { xPayload =
                              ( xConn
                              , Vessel.OpponentJoined $ Game.toElm newGame
                              )
                          , oPayload =
                              ( conn
                              , Vessel.RegistrationSuccessful
                              )
                          }
                        -- }}}
                      Nothing ->
                        -- {{{
                        OneVessel conn Vessel.RegistrationSuccessful
                        -- }}}
                  , newGame
                  )
                  -- }}}
                (Nothing, Just oP) ->
                  -- {{{
                  let
                    newGame =
                      Game info $ players
                        { Player.xPlayer =
                            Just $ Player playerTag (Just conn)
                        }
                  in
                  ( case Player.conn oP of
                      Just oConn ->
                        -- {{{
                        TwoVessels
                          { xPayload =
                              ( conn
                              , Vessel.RegistrationSuccessful
                              )
                          , oPayload =
                              ( oConn
                              , Vessel.OpponentJoined $ Game.toElm newGame
                              )
                          }
                        -- }}}
                      Nothing ->
                        -- {{{
                        OneVessel conn Vessel.RegistrationSuccessful
                        -- }}}
                  , newGame
                  )
                  -- }}}
                (Just _, Just _) ->
                  -- {{{
                  ( OneVessel conn Vessel.GameIsFull
                  , g
                  )
                  -- }}}
        )
        pool
        []
      -- }}}
    _ ->
      -- {{{
      (OneVessel conn Vessel.Empty, pool)
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
  let (responseWrappedVessel, newPool) =
        updatePoolFrom conn gameCode receivedVessel allGames
  case responseWrappedVessel of
    OneVessel destConn v ->
      liftIO $ WS.sendTextData destConn v
    TwoVessels (xConn, xV) (oConn, oV) ->
      liftIO 
        ( WS.sendTextData xConn xV >> WS.sendTextData oConn oV
        )
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
  :<|> serveDirectoryWith
         ( let
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
  serve api $ hoistServer api (`runReaderT` appState) server

startApp :: IO ()
startApp = do
  initAppState <- initApp
  putStrLn "\n============================"
  putStrLn "Starting server on port 8080"
  putStrLn "============================\n"
  run 8080 (app initAppState)
-- }}}
