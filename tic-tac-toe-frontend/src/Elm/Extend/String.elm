module Extend.String exposing (..)


twoDigits : Int -> String
twoDigits int =
  if int < 10 then
    "0" ++ String.fromInt int
  else
    String.fromInt int


timerFromSeconds : Int -> String
timerFromSeconds totSecs =
  let
    mins = totSecs // 60
    secs = modBy 60 totSecs
  in
  twoDigits mins ++ ":" ++ twoDigits secs
