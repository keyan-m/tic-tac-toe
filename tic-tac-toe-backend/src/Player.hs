{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}

module Player where

import ClassyPrelude hiding (join)
import Data.Function ((&))
import Elm.Derive
import qualified Network.WebSockets as WS

data Player = Player
  { tag   :: String
  , conn  :: Maybe WS.Connection
  }

data ElmPlayer = ElmPlayer
  { elmTag      :: String
  , isConnected :: Bool
  } deriving (Generic, Show)
deriveBoth defaultOptions ''ElmPlayer


data Players = Players
  { xPlayer :: Maybe Player
  , oPlayer :: Maybe Player
  }

data ElmPlayers = ElmPlayers
  { xElmPlayer :: Maybe ElmPlayer
  , oElmPlayer :: Maybe ElmPlayer
  } deriving (Generic, Show)
deriveBoth defaultOptions ''ElmPlayers


fromElm :: ElmPlayer -> Player
fromElm (ElmPlayer theTag _) =
  -- {{{
  Player
    { tag  = theTag
    , conn = Nothing
    }
  -- }}}

toElm :: Player -> ElmPlayer
toElm player =
  -- {{{
  ElmPlayer
    { elmTag      = tag player
    , isConnected = isJust $ conn player
    }
  -- }}}


fromElms :: ElmPlayers -> Players
fromElms ps =
  Players
    { xPlayer = fmap fromElm (xElmPlayer ps)
    , oPlayer = fmap fromElm (oElmPlayer ps)
    }

toElms :: Players -> ElmPlayers
toElms ps =
  ElmPlayers
    { xElmPlayer = fmap toElm (xPlayer ps)
    , oElmPlayer = fmap toElm (oPlayer ps)
    }



