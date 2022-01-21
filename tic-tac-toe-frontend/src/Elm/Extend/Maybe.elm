module Extend.Maybe exposing (..)

isJust : Maybe a -> Bool
isJust mA =
  case mA of
    Just _ ->
      True
    Nothing ->
      False
