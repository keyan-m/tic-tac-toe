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
     = "new"   :> ReqBody '[JSON] String
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
  :<|> "play"  :> WebSocketPending
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


broadcastGame :: Game -> IO ()
broadcastGame game@(Game info Players {xPlayer = mXP, oPlayer = mOP}) =
  -- {{{
  let
    broadcastGameToPlayer player =
      -- {{{
      -- case trace ("BROADCASTING:\n" ++ show (Game.toElm game)) (Player.conn player) of
      case Player.conn player of
        Nothing ->
          return ()
        Just conn ->
            Game.toElm game
          & Vessel.GameStateUpdate
          & WS.sendTextData conn
      -- }}}
  in
  case (mXP, mOP) of
    (Nothing, Nothing) ->
      return ()
    (Just xP, Nothing) ->
      broadcastGameToPlayer xP
    (Nothing, Just oP) ->
      broadcastGameToPlayer oP
    (Just xP, Just oP) ->
      broadcastGameToPlayer xP >> broadcastGameToPlayer oP
  -- }}}
-- }}}



-- HANDLERS
-- {{{
newGameHandler :: String -> ApiHandler ElmGame
newGameHandler xPTag = do
  -- {{{
  appState <- ask -- :: ReaderT r m r
  allGames <- getGames appState
  newGameCode <- uniqueRandom6Chars (map Game.getCode allGames)
  let newGame = Game.new newGameCode xPTag
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
               -> Vessel
               -> [Game]
               -> (WrappedVessel, [Game])
updatePoolFrom conn givenVes pool =
  -- {{{
  let
    go :: String
       -> (Game -> (WrappedVessel, Game))
       -> [Game]
       -> [Game]
       -> (WrappedVessel, [Game])
    go targetCode updateFn remainingGames soFar =
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
            go targetCode updateFn gs (g : soFar)
            --                        ^---------^
            -- order doesn't seem to matter yet.
          -- }}}
      -- }}}
  in
  case givenVes of
    Vessel.Empty ->
      -- {{{
      (OneVessel conn Vessel.Empty, pool)
      -- }}}
    Vessel.RegistrationRequest targetCode (ElmPlayer playerTag False) ->
      -- {{{
      go
        targetCode
        ( \g@(Game info players@Players {xPlayer = mXP, oPlayer = mOP}) ->
              let
                newGameFromNewPlayer newIsX newP =
                  -- {{{
                  let
                    newPs =
                      if newIsX then
                        players {Player.xPlayer = newP}
                      else
                        players {Player.oPlayer = newP}
                  in
                  Game info newPs
                  -- }}}
                onlyOne forX theP =
                  -- {{{
                  if   (Player.conn theP & isJust)
                    || (Player.tag theP /= playerTag)
                  then
                    ( OneVessel conn Vessel.InvalidRequest
                    , g
                    )
                  else
                    let
                      newP = Just $ Player playerTag (Just conn)
                      newGame = newGameFromNewPlayer forX newP
                      newElmGame = Game.toElm newGame
                    in
                    ( trace (show newElmGame) $ OneVessel
                        conn
                        (Vessel.RegistrationSuccessful newElmGame)
                    , newGame
                    )
                  -- }}}
              in
              case (mXP, mOP) of
                (Nothing, Nothing) ->
                  -- {{{
                  ( OneVessel conn Vessel.InvalidRequest
                  , g
                  )
                  -- }}}
                (Just xP, Nothing) ->
                  -- {{{
                  onlyOne True xP
                  -- }}}
                (Nothing, Just oP) ->
                  -- {{{
                  onlyOne False oP
                  -- }}}
                (Just xP, Just oP) ->
                  -- {{{
                  let
                    mXConn = Player.conn xP
                    mOConn = Player.conn oP
                    regOneAndInformTheOther regX otherConn =
                      -- {{{
                      let
                        newP = Just $ Player playerTag (Just conn)
                        newGame = newGameFromNewPlayer regX newP
                        newElmGame = Game.toElm newGame
                        vForAlreadyRegd =
                          (otherConn, Vessel.OpponentJoined newElmGame)
                        vForNewReg =
                          (conn, Vessel.RegistrationSuccessful newElmGame)
                      in
                      ( if regX then
                          TwoVessels
                            { xPayload = vForNewReg
                            , oPayload = vForAlreadyRegd
                            }
                        else
                          TwoVessels
                            { xPayload = vForAlreadyRegd
                            , oPayload = vForNewReg
                            }
                      , newGame
                      )
                      -- }}}
                  in
                  case (mXConn, mOConn) of
                    (Just _, Just _) ->
                      -- {{{
                      ( OneVessel conn Vessel.InvalidRequest
                      , g
                      )
                      -- }}}
                    (Just xConn, Nothing) ->
                      -- {{{
                      regOneAndInformTheOther False xConn
                      -- }}}
                    (Nothing, Just oConn) ->
                      -- {{{
                      regOneAndInformTheOther True oConn
                      -- }}}
                    (Nothing, Nothing) ->
                      -- {{{
                      if playerTag == Player.tag xP then
                        onlyOne True xP
                      else if playerTag == Player.tag oP then
                        onlyOne False oP
                      else
                        ( OneVessel conn Vessel.InvalidRequest
                        , g
                        )
                      -- }}}
                  -- }}}
        )
        pool
        []
      -- }}}
    Vessel.PlayerLeaving targetCode (ElmPlayer playerTag True) ->
      -- {{{
      go
        targetCode
        ( \g@(Game info players@Players {xPlayer = mXP, oPlayer = mOP}) ->
            case (mXP, mOP) of
              (Just (Player xTag mXConn), Just (Player oTag mOConn)) ->
                -- {{{
                let
                  xIsLeaving = xTag == playerTag
                  oIsLeaving = oTag == playerTag
                in
                case (xIsLeaving, mXConn, oIsLeaving, mOConn) of
                  (_, Just xConn, True, _) ->
                    -- {{{
                    ( OneVessel xConn Vessel.OpponentLeft
                    , Game info $ players {oPlayer = Nothing}
                    )
                    -- }}}
                  (True, _, _, Just oConn) ->
                    -- {{{
                    ( OneVessel oConn Vessel.OpponentLeft
                    , Game info $ players {xPlayer = Nothing}
                    )
                    -- }}}
                  _ ->
                    -- {{{
                    ( OneVessel conn Vessel.InvalidRequest
                    , g
                    )
                    -- }}}
                -- }}}
              _ ->
                -- {{{
                ( OneVessel conn Vessel.InvalidRequest
                , g
                )
                -- }}}
        )
        pool
        []
      -- }}}
    Vessel.GameStateRequest gCode pTag ->
      -- {{{
      let
        findGameAndRespond remainingGames =
          -- {{{
          case remainingGames of
            [] ->
              -- {{{
              ( OneVessel conn Vessel.InvalidRequest
              , pool
              )
              -- }}}
            g@(Game info players@Players {xPlayer = mXP, oPlayer = mOP}) : gs ->
              -- {{{
              if Game.gameCode info == gCode then
                -- {{{
                let
                  check (Just (Player itsTag (Just itsConn))) =
                    if itsTag == pTag then
                      Just $
                        OneVessel
                          itsConn
                          (Vessel.GameStateUpdate $ Game.toElm g)
                    else
                      Nothing
                  check _ = Nothing
                in
                case (check mXP, check mOP) of
                  (Nothing, Nothing) ->
                    findGameAndRespond gs
                  (Just output, _) ->
                    ( output
                    , pool
                    )
                  (_, Just output) ->
                    ( output
                    , pool
                    )
                -- }}}
              else
                -- {{{
                findGameAndRespond gs
                -- }}}
              -- }}}
          -- }}}
      in
      findGameAndRespond pool
      -- }}}
    _ ->
      -- {{{
      (OneVessel conn Vessel.Empty, pool)
      -- }}}
  -- }}}


wsHandler :: WS.PendingConnection -> ApiHandler ()
wsHandler pendingConn = do
  -- {{{
  appState <- ask
  allGames <- getGames appState
  --
  conn     <- liftIO $ WS.acceptRequest pendingConn
  liftIO $ WS.withPingThread conn 1
    (return ()) $ do
    forever $ do
      receivedVessel <- WS.receiveData conn
      --                ^------------^ can throw ConnectionClosed
      let (responseWrappedVessel, newPool) =
            updatePoolFrom conn receivedVessel allGames
      atomically $ writeTVar (games appState) newPool
      case responseWrappedVessel of
        OneVessel destConn v ->
          WS.sendTextData destConn (trace ("ONE V:\n" ++ show v) v)
        TwoVessels (xConn, xV) (oConn, oV) ->
             WS.sendTextData xConn (trace ("X:\n" ++ show xV) xV)
          >> WS.sendTextData oConn (trace ("O:\n" ++ show oV) oV)
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
