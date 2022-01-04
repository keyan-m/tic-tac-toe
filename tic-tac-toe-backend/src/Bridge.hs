module Bridge (run) where


import Elm.Module

import Data.Proxy
import System.IO (writeFile)

import qualified Player
import qualified Game
import qualified Join
import qualified Vessel


run :: IO ()
run = do
  writeFile "../tic-tac-toe-frontend/src/Elm/Bridge.elm" $ makeElmModule "Bridge"
    [ DefineElm (Proxy :: Proxy Game.Mark)
    , DefineElm (Proxy :: Proxy Game.Playground)
    , DefineElm (Proxy :: Proxy Player.ElmPlayer)
    , DefineElm (Proxy :: Proxy Player.ElmPlayers)
    , DefineElm (Proxy :: Proxy Game.Info)
    , DefineElm (Proxy :: Proxy Game.ElmGame)
    , DefineElm (Proxy :: Proxy Join.ElmResult)
    , DefineElm (Proxy :: Proxy Vessel.Vessel)
    ]
