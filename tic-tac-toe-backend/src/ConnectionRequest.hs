{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module ConnectionRequest where


import ClassyPrelude
import Elm.Derive
import Player (Player, ElmPlayer)
import qualified Player


data ConnectionRequest = ConnectionRequest
  { requestedGameCode :: String
  , requestingPlayer  :: Player
  }

data ElmConnectionRequest = ElmConnectionRequest
  { elmRequestedGameCode :: String
  , elmRequestingPlayer  :: ElmPlayer
  } deriving (Generic, Show, Eq)
deriveBoth defaultOptions ''ConnectionRequest


toElm :: ConnectionRequest -> ElmConnectionRequest
toElm req =
  -- {{{
  ElmConnectionRequest
    { elmRequestedGameCode = requestedGameCode req
    , elmRequestingPlayer  = Player.toElm $ requestingPlayer req
    }
  -- }}}


fromElm :: ElmConnectionRequest -> ConnectionRequest
fromElm elmReq =
  -- {{{
  ConnectionRequest
    { requestedGameCode = elmRequestedGameCode elmReq
    , requestingPlayer  = Player.fromElm $ elmRequestingPlayer elmReq
    }
  -- }}}


