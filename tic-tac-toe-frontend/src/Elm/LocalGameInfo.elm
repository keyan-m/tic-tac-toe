module LocalGameInfo exposing
  ( LocalGameInfo
  , init
  , update
  , Msg (..)
  , getLatestGameTime
  , getGameResult
  )


import Extend.List as List
import Game as Game exposing (GameResult)
--
import Linear as Linear


type TimeTrend
  = NoTrend
  | MidGameTrend (Float -> Float)
  | FinalGameTime Int

type LocalGameInfo = LocalGameInfo
  { mResult               : Maybe GameResult
  , localStartPOSIX       : Int
  , localLatestPOSIX      : Int
  , localServerPOSIXPairs : List (Float, Float)
  , timeTrend             : TimeTrend
  }


init : Int -> Maybe Int -> LocalGameInfo
init initX0 mY0 =
  -- {{{
  let
    ((lsp, llp), lspp, tt) =
      -- {{{
      case mY0 of
        Just initY0 ->
          let
            (x0, y0) = (toFloat initX0, toFloat initY0)
          in
          ( (initX0, initX0)
          , [(x0, y0)]
          , MidGameTrend (\x -> x + (y0 - x0))
          )
        Nothing ->
          ( (0, 0)
          , []
          , NoTrend
          )
      -- }}}
  in
  LocalGameInfo
    { mResult               = Nothing
    , localStartPOSIX       = lsp
    , localLatestPOSIX      = llp
    , localServerPOSIXPairs = lspp
    , timeTrend             = tt
    }
  -- }}}


type Msg
  = Conclude (Int, Int) GameResult
  | NewPair  (Int, Int)
  | SetLocalLatestPOSIX Int

update : Msg -> LocalGameInfo -> LocalGameInfo
update msg ((LocalGameInfo info) as lgi) =
  case msg of
    Conclude ((_, lastY) as newPair) result ->
      -- {{{
      let
        (LocalGameInfo newInfo) = update (NewPair newPair) lgi
      in
      LocalGameInfo
        { newInfo
        | mResult = Just result
        , timeTrend = FinalGameTime lastY
        }
      -- }}}
    NewPair (newX, newY) ->
      -- {{{
      let
        newPairs =
          (toFloat newX / 100000, toFloat newY / 100000)
          :: info.localServerPOSIXPairs
      in
      LocalGameInfo
        { info
        | localServerPOSIXPairs = newPairs
        , localLatestPOSIX      = newX
        , timeTrend             = MidGameTrend (Linear.regression newPairs)
        }
      -- }}}
    SetLocalLatestPOSIX latest ->
      -- {{{
      LocalGameInfo
        { info
        | localLatestPOSIX = latest
        }
      -- }}}


getLatestGameTime : Int -> LocalGameInfo -> Maybe Int
getLatestGameTime serverStartPOSIX (LocalGameInfo info) =
  -- {{{
  case info.timeTrend of
    NoTrend ->
      Nothing
    MidGameTrend trend ->
      (trend (toFloat info.localLatestPOSIX)) - (toFloat serverStartPOSIX)
      |> round
      |> Just
    FinalGameTime finalPOSIX ->
      (toFloat finalPOSIX) - (toFloat serverStartPOSIX)
      |> round
      |> Just
  -- }}}


getGameResult : LocalGameInfo -> Maybe GameResult
getGameResult (LocalGameInfo info) = info.mResult
