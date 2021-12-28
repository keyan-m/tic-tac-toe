{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}


module Vessel where


import ClassyPrelude hiding (join)
import Data.Function ((&))
import Data.Aeson (encode, decode)
import Network.WebSockets (WebSocketsData)
import qualified Network.WebSockets as WS
import Elm.Derive
import Player (ElmPlayer)


data Vessel
  = Empty
  | XPlayerRegistration ElmPlayer
  | OPlayerJoinRequest  ElmPlayer
  deriving (Generic, Show)

deriveBoth defaultOptions ''Vessel

instance WebSocketsData Vessel where
  -- {{{
  fromLazyByteString bs =
    decode bs & fromMaybe Empty

  toLazyByteString =
    encode

  fromDataMessage dM =
    case dM of
      WS.Text byteStr _ ->
        fromLazyByteString byteStr
      WS.Binary byteStr ->
        fromLazyByteString byteStr
  -- }}}
