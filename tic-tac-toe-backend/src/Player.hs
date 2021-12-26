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

newtype ElmPlayer = ElmPlayer String
deriving (Generig, Show)
deriveBoth defaultOptions ''ElmPlayer


data Players = Players
  { xPlayer :: Maybe Player
  , oPlayer :: Maybe Player
  }
instance Functor Players where
  fmap f players = Players
    { xPlayer = fmap f (xPlayer players)
    , oPlayer = fmap f (oPlayer players)
    }

data ElmPlayers = ElmPlayers
  { xElmPlayer :: Maybe ElmPlayer
  , oElmPlayer :: Maybe ElmPlayer
  } deriving (Generic, Show)
deriveBoth defaultOptions ''ElmPlayers

instance Functor ElmPlayers where
  fmap f players = ElmPlayers
    { xElmPlayer = fmap f (xElmPlayer players)
    , oElmPlayer = fmap f (oElmPlayer players)
    }


fromElm :: ElmPlayer -> Player
fromElm (ElmPlayer theTag) =
  -- {{{
  Player
    { tag  = theTag
    , conn = Nothing
    }
  -- }}}

toElm :: Player -> ElmPlayer
toElm player =
  -- {{{
  ElmPlayer $ tag player
  -- }}}


fromElms :: ElmPlayers -> Players
fromElms = fmap fromElm

toElms :: Players -> ElmPlayers
toElms = fmap toElm


