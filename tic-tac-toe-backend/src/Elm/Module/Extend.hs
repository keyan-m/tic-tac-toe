module Elm.Module.Extend (makeElmModuleWithImports) where


import Data.Proxy

import Elm.Json
import Elm.TyRender
import Elm.TyRep
import Elm.Versions
import Elm.Module


makeElmModuleWithImports :: String
                         -> [String]
                         -> [DefineElm]
                         -> String
makeElmModuleWithImports moduleName imports defs =
  unlines
    ( [ moduleHeader Elm0p19 moduleName
      , ""
      , "import Json.Decode"
      , "import Json.Encode exposing (Value)"
      , "-- The following module comes from bartavelle/json-helpers"
      , "import Json.Helpers exposing (..)"
      , "import Dict exposing (Dict)"
      , "import Set exposing (Set)"
      , ""
      ] ++ map (\modName -> "import " ++ modName ++ " exposing (..)") imports
        ++ ["", ""]
    )
  ++ makeModuleContent defs
