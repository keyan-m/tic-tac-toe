module Bridge (run) where


import Elm.Module
import Elm.Module.Extend

import Data.Proxy
import System.IO (writeFile)

import qualified Player
import qualified Game
import qualified Join
import qualified Vessel


run :: IO ()
run = do
  writeFile "../tic-tac-toe-frontend/src/Elm/Player.elm" $ makeElmModule "Player"
    [ DefineElm (Proxy :: Proxy Player.ElmPlayer)
    , DefineElm (Proxy :: Proxy Player.ElmPlayers)
    ]
  writeFile "../tic-tac-toe-frontend/src/Elm/Game.elm" $
    makeElmModuleWithImports
      "Game"
      [ "Player"
      ]
      [ DefineElm (Proxy :: Proxy Game.ElmGame)
      , DefineElm (Proxy :: Proxy Game.Info)
      , DefineElm (Proxy :: Proxy Game.Mark)
      , DefineElm (Proxy :: Proxy Game.Playground)
      ]
  writeFile "../tic-tac-toe-frontend/src/Elm/Join.elm" $
    makeElmModuleWithImports
      "Join"
      [ "Player"
      , "Game"
      ]
      [ DefineElm (Proxy :: Proxy Join.ElmResult)
      ]
  writeFile "../tic-tac-toe-frontend/src/Elm/Vessel.elm" $
    makeElmModuleWithImports
      "Vessel"
      [ "Player"
      , "Game"
      ]
      [ DefineElm (Proxy :: Proxy Vessel.Vessel)
      ]
