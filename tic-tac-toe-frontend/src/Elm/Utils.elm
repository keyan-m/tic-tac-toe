module Utils exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE exposing
  ( onClick
  , preventDefaultOn
  )
import Html.Lazy as H
import Svg
import Svg.Attributes as SA
import Http


duration = "duration-200"
durationMillis = 200
timeoutMillis  = 20000
gameTimeTickMillis = 1000


type MessageType
  = SuccessMessage
  | WarningMessage
  | FailureMessage
  | NeutralMessage

type alias Message =
  { messageType : MessageType
  , body        : String
  }



httpErrorToString : Http.Error -> String
httpErrorToString err =
  -- {{{
  case err of
    Http.BadUrl str ->
      "BadUrl: " ++ str
    Http.Timeout ->
      "Timeout"
    Http.NetworkError ->
      "NetworkError"
    Http.BadStatus code ->
      "BadStatus: " ++ (String.fromInt code)
    Http.BadBody str ->
      "BadBody: " ++ str
  -- }}}



svgDefs : Html msg
svgDefs =
  -- {{{
  Svg.svg
    [ HA.attribute "aria-hidden" "true"
    , HA.attribute "style" "position: absolute; width: 0; height: 0; overflow: hidden;"
    , SA.version "1.1"
    , HA.attribute "xmlns" "http://www.w3.org/2000/svg"
    , HA.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
    ]
    [ Svg.defs []
        [ Svg.symbol [SA.id "icon-o", SA.viewBox "0 0 128 128"]
            -- {{{
            [ Svg.path
                [SA.d "M115.6 42.2c-2.825-6.675-6.862-12.663-12-17.8s-11.125-9.175-17.8-12c-6.912-2.912-14.237-4.4-21.8-4.4s-14.888 1.488-21.8 4.4c-6.675 2.825-12.663 6.863-17.8 12s-9.175 11.125-12 17.8c-2.912 6.913-4.4 14.237-4.4 21.8s1.488 14.888 4.4 21.8c2.825 6.675 6.863 12.662 12 17.8s11.125 9.175 17.8 12c6.913 2.912 14.237 4.4 21.8 4.4s14.888-1.487 21.8-4.4c6.675-2.825 12.662-6.862 17.8-12s9.175-11.125 12-17.8c2.925-6.912 4.4-14.25 4.4-21.8 0-7.563-1.487-14.888-4.4-21.8zM64 104c-22.050 0-40-17.95-40-40s17.95-40 40-40c22.050 0 40 17.95 40 40s-17.95 40-40 40z"]
                []
            ]
            -- }}}
        , Svg.symbol [SA.id "icon-x", SA.viewBox "0 0 128 128"]
            -- {{{
            [ Svg.path
                [SA.d "M75.313 64l42.338-42.337c3.125-3.125 3.125-8.188 0-11.313s-8.188-3.125-11.313 0l-42.338 42.337-42.337-42.35c-3.125-3.125-8.188-3.125-11.313 0s-3.125 8.188 0 11.313l42.337 42.35-42.35 42.338c-3.125 3.125-3.125 8.188 0 11.313 1.563 1.575 3.613 2.35 5.662 2.35s4.1-0.775 5.662-2.338l42.338-42.35 42.338 42.338c1.563 1.563 3.612 2.338 5.662 2.338s4.1-0.775 5.662-2.338c3.125-3.125 3.125-8.188 0-11.313l-42.35-42.338z"]
                []
            ]
            -- }}}
        ]
    ]
  -- }}}


viewSquareSvg : String -> String -> Html msg
viewSquareSvg edgeSize svgID =
  -- {{{
  Svg.svg
    [ SA.width  edgeSize
    , SA.height edgeSize
    , SA.class duration
    , SA.class "block fill-current"
    ]
    [ Svg.use [SA.xlinkHref svgID] [] 
    ]
  -- }}}

view64pxSvg : String -> Html msg
view64pxSvg =
  -- {{{
  viewSquareSvg "64px"
  -- }}}

view128pxSvg : String -> Html msg
view128pxSvg =
  -- {{{
  viewSquareSvg "128px"
  -- }}}

viewX : Html msg
viewX = view128pxSvg "#icon-x"

viewO : Html msg
viewO = view128pxSvg "#icon-o"

viewGameX : Html msg
viewGameX = view64pxSvg "#icon-x"

viewGameO : Html msg
viewGameO = view64pxSvg "#icon-o"


type alias ColorScheme =
  { bg         : String
  , txt        : String
  , grey       : String
  , bgInv      : String
  , btnBg      : String
  , btnTxt     : String
  , inputBg    : String
  , inputTxt   : String
  , inputFocus : String
  }

makeColorScheme seed =
  case modBy 9 seed of
    0 ->
      -- {{{ SLATE 
      { bg         = "bg-slate-700"
      , txt        = "text-slate-200"
      , grey       = "bg-slate-500"
      , bgInv      = "bg-slate-200"
      , btnBg      = "hover:bg-slate-300 bg-slate-400 active:bg-slate-500"
      , btnTxt     = "text-slate-900 active:text-slate-300"
      , inputBg    = "bg-slate-100"
      , inputTxt   = "text-slate-700"
      , inputFocus = "outline-0 focus:outline-1 outline-slate-500"
      }
      -- }}}
    1 ->
      -- {{{ STONE 
      { bg         = "bg-stone-700"
      , txt        = "text-stone-200"
      , grey       = "bg-stone-500"
      , bgInv      = "bg-stone-200"
      , btnBg      = "hover:bg-stone-300 bg-stone-400 active:bg-stone-500"
      , btnTxt     = "text-stone-900 active:text-stone-300"
      , inputBg    = "bg-stone-100"
      , inputTxt   = "text-stone-700"
      , inputFocus = "outline-0 focus:outline-1 outline-stone-500"
      }
      -- }}}
    2 ->
      -- {{{ AMBER 
      { bg         = "bg-amber-700"
      , txt        = "text-amber-200"
      , grey       = "bg-amber-500"
      , bgInv      = "bg-amber-200"
      , btnBg      = "hover:bg-amber-300 bg-amber-400 active:bg-amber-500"
      , btnTxt     = "text-amber-900 active:text-amber-300"
      , inputBg    = "bg-amber-100"
      , inputTxt   = "text-amber-700"
      , inputFocus = "outline-0 focus:outline-1 outline-amber-500"
      }
      -- }}}
    3 ->
      -- {{{ LIME 
      { bg         = "bg-lime-700"
      , txt        = "text-lime-200"
      , grey       = "bg-lime-500"
      , bgInv      = "bg-lime-200"
      , btnBg      = "hover:bg-lime-300 bg-lime-400 active:bg-lime-500"
      , btnTxt     = "text-lime-900 active:text-lime-300"
      , inputBg    = "bg-lime-100"
      , inputTxt   = "text-lime-700"
      , inputFocus = "outline-0 focus:outline-1 outline-lime-500"
      }
      -- }}}
    4 ->
      -- {{{ TEAL 
      { bg         = "bg-teal-700"
      , txt        = "text-teal-200"
      , grey       = "bg-teal-500"
      , bgInv      = "bg-teal-200"
      , btnBg      = "hover:bg-teal-300 bg-teal-400 active:bg-teal-500"
      , btnTxt     = "text-teal-900 active:text-teal-300"
      , inputBg    = "bg-teal-100"
      , inputTxt   = "text-teal-700"
      , inputFocus = "outline-0 focus:outline-1 outline-teal-500"
      }
      -- }}}
    5 ->
      -- {{{ SKY 
      { bg         = "bg-sky-700"
      , txt        = "text-sky-200"
      , grey       = "bg-sky-500"
      , bgInv      = "bg-sky-200"
      , btnBg      = "hover:bg-sky-300 bg-sky-400 active:bg-sky-500"
      , btnTxt     = "text-sky-900 active:text-sky-300"
      , inputBg    = "bg-sky-100"
      , inputTxt   = "text-sky-700"
      , inputFocus = "outline-0 focus:outline-1 outline-sky-500"
      }
      -- }}}
    6 ->
      -- {{{ INDIGO 
      { bg         = "bg-indigo-700"
      , txt        = "text-indigo-200"
      , grey       = "bg-indigo-500"
      , bgInv      = "bg-indogo-200"
      , btnBg      = "hover:bg-indigo-300 bg-indigo-400 active:bg-indigo-500"
      , btnTxt     = "text-indigo-900 active:text-indigo-300"
      , inputBg    = "bg-indigo-100"
      , inputTxt   = "text-indigo-700"
      , inputFocus = "outline-0 focus:outline-1 outline-indigo-500"
      }
      -- }}}
    7 ->
      -- {{{ PURPLE 
      { bg         = "bg-purple-700"
      , txt        = "text-purple-200"
      , grey       = "bg-purple-500"
      , bgInv      = "bg-purple-200"
      , btnBg      = "hover:bg-purple-300 bg-purple-400 active:bg-purple-500"
      , btnTxt     = "text-purple-900 active:text-purple-300"
      , inputBg    = "bg-purple-100"
      , inputTxt   = "text-purple-700"
      , inputFocus = "outline-0 focus:outline-1 outline-purple-500"
      }
      -- }}}
    _ ->
      -- {{{ PINK 
      { bg         = "bg-pink-700"
      , txt        = "text-pink-200"
      , grey       = "bg-pink-500"
      , bgInv      = "bg-pink-200"
      , btnBg      = "hover:bg-pink-300 bg-pink-400 active:bg-pink-500"
      , btnTxt     = "text-pink-900 active:text-pink-300"
      , inputBg    = "bg-pink-100"
      , inputTxt   = "text-pink-700"
      , inputFocus = "outline-0 focus:outline-1 outline-pink-500"
      }
      -- }}}
