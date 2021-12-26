module Bridge (run) where


import Elm.Module

import Data.Proxy
import System.IO (writeFile)

import qualified ConnectionRequest as CR
import qualified Player
import qualified Game
import qualified Vessel


run :: IO ()
run = do
  writeFile "../tic-tac-toe-frontend/src/Elm/Bridge.elm" $ makeElmModule "Bridge"
    [ DefineElm (Proxy :: Proxy CR.ConnectionRequest)
    , DefineElm (Proxy :: Proxy Game.Mark)
    , DefineElm (Proxy :: Proxy Game.Playground)
    , DefineElm (Proxy :: Proxy Player.ElmPlayer)
    , DefineElm (Proxy :: Proxy Game.GameInfo)
    , DefineElm (Proxy :: Proxy Game.Game)
    , DefineElm (Proxy :: Proxy Game.MaybeGame)
    , DefineElm (Proxy :: Proxy Vessel.ConnectionRequest)
    , DefineElm (Proxy :: Proxy Vessel.Vessel)
    ]
