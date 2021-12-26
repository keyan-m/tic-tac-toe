{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}


module Vessel where


import ClassyPrelude hiding (join)
import Data.Function ((&))
import Elm.Derive
import ConnectionRequest (ElmConnectionRequest)


data Vessel
  = XPlayerConnectionRequest ElmConnectionRequest
  | OPlayerConnectionRequest ElmConnectionRequest
deriveBoth defaultOptions ''Vessel
