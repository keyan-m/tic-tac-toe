port module Main exposing (..)


-- {{{ IMPORTS 
import Time
import Task
import Process
import Utils exposing (..)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg
import Svg.Attributes as SA
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Browser.Events as BE
import Url exposing (Url)
import Url.Builder as UB
import Json.Encode as E
import Json.Decode as D
import Json.Decode.Pipeline as D exposing (required, optional)
import Http
-- From Bridge.hs
import Player exposing (ElmPlayer, ElmPlayers)
import Game as Game exposing (ElmGame(..))
import Join as Join
import Vessel exposing (Vessel)
-- Decoder format: jsonDec<datatype>
-- Encoder format: jsonEnc<datatype>
-- }}}


-- {{{ MODEL 

main = Browser.application
  { init          = init
  , view          = view
  , update        = update
  , subscriptions = subscriptions
  , onUrlRequest  = always NoOp
  , onUrlChange   = always NoOp
  }


type alias Flags =
  { title    : String
  , protocol : String
  , host     : String
  , widthPx  : Int
  , heightPx : Int
  , darkMode : Bool
  , seed     : Int
  }
-- {{{ JSON 
flagsToJSON : Flags -> E.Value
flagsToJSON flags =
  -- {{{
  E.object
    [ ("title"   , E.string flags.title)
    , ("protocol", E.string flags.protocol)
    , ("host"    , E.string flags.host)
    , ("widthPx" , E.int flags.widthPx)
    , ("heightPx", E.int flags.heightPx)
    , ("darkMode", E.bool flags.darkMode)
    , ("seed"    , E.int flags.seed)
    ]
  -- }}}
flagsDecoder : D.Decoder Flags
flagsDecoder =
  -- {{{
  D.succeed Flags
  |> required "title"    D.string
  |> required "protocol" D.string
  |> required "host"     D.string
  |> required "widthPx"  D.int
  |> required "heightPx" D.int
  |> required "darkMode" D.bool
  |> required "seed"     D.int
  -- }}}
-- }}}

type Page
  = LandingPage LandingPrompt
  | GamePage    ElmGame

type LandingPrompt
  = NoPrompt
  | NewGamePrompt
  | JoinGamePrompt

type ProgramView
  = PageView      Page
  | FadingOut Int Page
  | FadingIn  Int Page

type alias Model =
  { programView : ProgramView
  , title       : String
  , messages    : List Message
  , gamerTag    : String
  , gameCode    : String
  , widthPx     : Int
  , heightPx    : Int
  , key         : Nav.Key
  , url         : Url
  , darkMode    : Bool
  , seed        : Int
  }


defaultPlayer1Tag = "Player 1"
defaultPlayer2Tag = "Player 2"


init : E.Value -> Url -> Nav.Key -> (Model, Cmd Msg)
init flagsVal url key = 
  -- {{{
  let
    default =
      { programView = PageView (LandingPage NoPrompt)
      , title       = "Tic-Tac-Toe"
      , messages    = []
      , gamerTag    = defaultPlayer1Tag
      , gameCode    = ""
      , widthPx     = 1920
      , heightPx    = 1080
      , key         = key
      , url         = url
      , darkMode    = False
      , seed        = 0
      }
  in
  case D.decodeValue flagsDecoder flagsVal of
    Ok flags ->
      -- {{{
      ( { default
        | title       = flags.title
        , widthPx     = flags.widthPx
        , heightPx    = flags.heightPx
        , darkMode    = flags.darkMode
        , seed        = flags.seed
        }
      , setBackgroundColor (makeColorScheme flags.seed |> .bg)
      )
      -- }}}
    Err err ->
      -- {{{
      ( default
      , setBackgroundColor (makeColorScheme 0 |> .bg)
      )
      -- }}}
  -- }}}
-- }}}


-- {{{ UPDATE 
type Msg
  = NoOp
  | RunCmd (Cmd Msg)
  | SetViewportDimensions Int Int
  | UpdateSeedWith (Int -> Int)
  | ToggleNewGamePrompt
  | ToggleJoinGamePrompt
  | UpdateGamerTag String
  | UpdateGameCode String
  | StartFadingOut Int
  | StartFadingInNewPage Page Int
  | PutUpNewPage Page Int
  | RequestNewGameCode
  | HandleNewGameCodeResponse (Result Http.Error ElmGame)
  | CloseServer
  | RequestJoinGame
  | HandleJoinGameResponse (Result Http.Error Join.ElmResult)
  | HandleVessel (Result D.Error Vessel)


noCmd : Model -> (Model, Cmd Msg)
noCmd model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  -- {{{
  case msg of
    NoOp ->
      -- {{{
      noCmd model
      -- }}}
    RunCmd cmd ->
      -- {{{
      (model, cmd)
      -- }}}
    SetViewportDimensions newW newH ->
      -- {{{
      { model
      | widthPx  = newW
      , heightPx = newH
      }
      |> noCmd
      -- }}}
    UpdateSeedWith updateFn ->
      -- {{{
      let
        newSeed = updateFn model.seed
      in
      ( { model
        | seed = newSeed
        }
      , setBackgroundColor (makeColorScheme newSeed |> .bg)
      )
      -- }}}
    ToggleNewGamePrompt ->
      -- {{{
      case model.programView of
        PageView (LandingPage NewGamePrompt) ->
          -- {{{
          { model
          | programView = PageView (LandingPage NoPrompt)
          }
          |> noCmd
          -- }}}
        PageView (LandingPage _) ->
          -- {{{
          { model
          | programView = PageView (LandingPage NewGamePrompt)
          , gamerTag =
              if model.gamerTag == defaultPlayer2Tag then
                defaultPlayer1Tag
              else
                model.gamerTag
          }
          |> noCmd
          -- }}}
        _ ->
          -- {{{
          noCmd model
          -- }}}
      -- }}}
    ToggleJoinGamePrompt ->
      -- {{{
      case model.programView of
        PageView (LandingPage JoinGamePrompt) ->
          -- {{{
          { model
          | programView = PageView (LandingPage NoPrompt)
          }
          |> noCmd
          -- }}}
        PageView (LandingPage _) ->
          -- {{{
          { model
          | programView = PageView (LandingPage JoinGamePrompt)
          , gamerTag =
              if model.gamerTag == defaultPlayer1Tag then
                defaultPlayer2Tag
              else
                model.gamerTag
          }
          |> noCmd
          -- }}}
        _ ->
          -- {{{
          noCmd model
          -- }}}
      -- }}}
    UpdateGamerTag newTag ->
      -- {{{
      {model | gamerTag = newTag}
      |> noCmd
      -- }}}
    UpdateGameCode newCode ->
      -- {{{
      {model | gameCode = String.toUpper newCode}
      |> noCmd
      -- }}}
    StartFadingOut startOfFade ->
      -- {{{
      { model
      | programView = FadingOut startOfFade (getCurrentPage model.programView)
      }
      |> noCmd
      -- }}}
    StartFadingInNewPage newPage currTime ->
      -- {{{
      case model.programView of
        FadingOut startOfFade page ->
          -- {{{
          let
            millisElapsed = currTime - startOfFade
          in
          if millisElapsed >= durationMillis then
            -- {{{
            ( { model
              | programView = FadingIn currTime newPage
              }
            , giveTimeToMsg <| PutUpNewPage newPage
            )
            -- }}}
          else
            -- {{{
            ( model
            , updateAfter
                (durationMillis - millisElapsed)
                (StartFadingInNewPage newPage (startOfFade + durationMillis))
            )
            -- }}}
          -- }}}
        _ ->
          -- {{{
          { model
          | programView = PageView newPage
          }
          |> noCmd
          -- }}}
      -- }}}
    PutUpNewPage newPage currTime ->
      -- {{{
      case model.programView of
        FadingIn startOfFade page ->
          -- {{{
          let
            millisElapsed = currTime - startOfFade
          in
          if millisElapsed >= durationMillis then
            -- {{{
            { model
            | programView = PageView newPage
            }
            |> noCmd
            -- }}}
          else
            -- {{{
            ( model
            , updateAfter
                (durationMillis - millisElapsed)
                (PutUpNewPage newPage (startOfFade + durationMillis))
            )
            -- }}}
          -- }}}
        _ ->
          -- {{{
          { model
          | programView = PageView newPage
          }
          |> noCmd
          -- }}}
      -- }}}
    RequestNewGameCode ->
      -- {{{
      ( model
      , Cmd.batch
          [ giveTimeToMsg StartFadingOut
          , httpPost
              (UB.absolute ["new"] [])
              (Http.jsonBody (E.string model.gamerTag))
              (Http.expectJson HandleNewGameCodeResponse Game.jsonDecElmGame)
          ]
      )
      -- }}}
    HandleNewGameCodeResponse res ->
      -- {{{
      case res of
        Ok ((ElmGame info players) as newGame) ->
          ( { model
            | gameCode = info.gameCode
            }
          , Cmd.batch
              [ Vessel.RegistrationRequest
                  info.gameCode
                  { elmTag      = model.gamerTag
                  , isConnected = False
                  }
                |> Vessel.jsonEncVessel
                |> openSocketAndSend
              , giveTimeToMsg <|
                  StartFadingInNewPage (GamePage newGame)
              ]
          )
        Err err ->
          ( { model
            | messages =
                (Message FailureMessage (httpErrorToString err))
                :: model.messages
            }
          , giveTimeToMsg <|
              StartFadingInNewPage (getCurrentPage model.programView)
          )
      -- }}}
    CloseServer ->
      -- {{{
      ( model
      , Cmd.batch
          [ giveTimeToMsg StartFadingOut
          , updateAfter durationMillis (UpdateGameCode "")
          , runCmdAfter durationMillis <|
              giveTimeToMsg (StartFadingInNewPage (LandingPage NoPrompt))
          , httpPost
              (UB.absolute ["close"] [])
              (Http.jsonBody (E.string model.gameCode))
              (Http.expectWhatever (always NoOp))
          , closeSocket ()
          ]
      )
      -- }}}
    RequestJoinGame ->
      -- {{{
      ( model
      , Cmd.batch
          [ giveTimeToMsg StartFadingOut
          , httpPost
              (UB.absolute ["join", model.gameCode] [])
              ( Player.jsonEncElmPlayer
                  { elmTag      = model.gamerTag
                  , isConnected = False
                  }
                |> Http.jsonBody
              )
              ( Http.expectJson
                  HandleJoinGameResponse
                  Join.jsonDecElmResult
              )
          ]
      )
      -- }}}
    HandleJoinGameResponse res ->
      -- {{{
      let
        failure errStr =
          ( { model
            | messages =
                (Message FailureMessage errStr)
                :: model.messages
            }
          , giveTimeToMsg <|
              StartFadingInNewPage (getCurrentPage model.programView)
          )
      in
      case res of
        Ok (Join.ElmSuccessful game) ->
          ( model
          , Cmd.batch
              [ Vessel.RegistrationRequest
                  model.gameCode
                  { elmTag      = model.gamerTag
                  , isConnected = False
                  }
                |> Vessel.jsonEncVessel
                |> openSocketAndSend
              , giveTimeToMsg <|
                  StartFadingInNewPage
                    ( GamePage game
                    )
              ]
          )
        Ok (Join.ElmFailed errStr) ->
          failure errStr
        Err err ->
          failure (httpErrorToString err)
      -- }}}
    HandleVessel res ->
      -- {{{
      case res of
        Ok vessel ->
          case vessel of
            Vessel.RegistrationSuccessful newGame ->
              { model
              | programView = PageView (GamePage newGame)
              }
              |> noCmd
            Vessel.OpponentJoined newGame ->
              { model
              | programView = PageView (GamePage newGame)
              }
              |> noCmd
            _ ->
              noCmd model
        Err _ ->
          noCmd model -- TODO
      -- }}}
  -- }}}


port setBackgroundColor : String  -> Cmd msg
port openSocketAndSend  : E.Value -> Cmd msg
port closeSocket        : ()      -> Cmd msg
-- port sendThroughSocket  : E.Value -> Cmd msg
-- }}}


-- {{{ VIEW 
type FadeState
  = NoFade
  | InFade
  | OutFade

viewInput : ColorScheme
         -> String
         -> String
         -> String
         -> (String -> Msg)
         -> Html Msg
viewInput colorScheme id lbl val msg =
  -- {{{
  H.div
    [ HA.class "flex flex-col flex-stretch"
    , HA.class "font-mono pb-4 relative"
    , HA.class "w-full"
    ]
    [ H.label
        [ HA.for id
        , HA.class colorScheme.txt
        , HA.class "text-center sm:text-left pb-2"
        ] [H.text lbl]
    , H.input
        [ HA.type_ "text"
        , HA.id id
        , HE.onInput msg
        , HA.value val
        , HA.class colorScheme.inputBg
        , HA.class colorScheme.inputTxt
        , HA.class "rounded-lg p-2"
        ] []
    ]
  -- }}}


buttonAttrs colorScheme =
  [ HA.class "flex items-center justify-center"
  , HA.class "py-2 px-8 font-mono rounded-lg text-center"
  , HA.class "shadow active:shadow-sm"
  , HA.class colorScheme.btnBg
  , HA.class colorScheme.btnTxt
  , HA.class duration
  , HA.class "transition-colors select-none"
  ]

viewFormButton : ColorScheme -> String -> Html msg
viewFormButton colorScheme lbl =
  -- {{{
  H.button
    ( HA.type_ "submit" :: buttonAttrs colorScheme
    ) [H.text lbl]
  -- }}}


viewLandingPage : ColorScheme -> Bool -> String -> String -> LandingPrompt -> Html Msg
viewLandingPage colorScheme fadingOut gamerTag gameCode landingPrompt =
  -- {{{
  let
    gamerTagInput _ =
      -- {{{
      viewInput
        colorScheme
        "gamerTag"
        "your game tag"
        gamerTag
        UpdateGamerTag
      -- }}}
    selectedClasses =
      -- {{{
      "opacity-100 scale-100 sm:scale-125"
      -- }}}
    disabledClasses =
      -- {{{
         "opacity-20 hover:opacity-70"
      ++ " scale-50 hover:scale-75"
      ++ " sm:scale-75 sm:hover:scale-75"
      -- }}}
    formMaker comps =
      -- {{{
      let
        commonAttrs =
          [ HA.class duration
          , HA.class "transition-margin-padding pl-0 shrink-0"
          ]
        commonPromptedAttrs =
             HA.class "flex flex-col items-stretch sm:pt-0"
          :: HA.class "justify-start sm:justify-center"
          :: commonAttrs
      in
      case landingPrompt of
        NoPrompt ->
          H.form (HA.class "pt-0 mt-0" :: commonAttrs) []
        NewGamePrompt ->
          H.form
            (    HA.class "pt-0 -mt-6 sm:mt-0"
              :: HE.onSubmit RequestNewGameCode
              :: commonPromptedAttrs
            )
            comps
        JoinGamePrompt ->
          H.form
            (    HA.class "pt-8 mt-0"
              :: HE.onSubmit RequestJoinGame
              :: commonPromptedAttrs
            )
            comps
      -- }}}
    xoMaker isX =
      -- {{{
      let
        {label, origin, svgIcon, msg} =
          -- {{{
          if isX then
            { label = "New Game"
            , origin = "origin-bottom sm:origin-right"
            , svgIcon = viewX
            , msg = ToggleNewGamePrompt
            }
          else
            { label = "Join Game"
            , origin = "origin-top sm:origin-left"
            , svgIcon = viewO
            , msg = ToggleJoinGamePrompt
            }
          -- }}}
      in
      H.div
        [ HA.class "font-mono"
        , HA.class "flex items-center justify-center"
        , HA.class duration
        , HA.class colorScheme.txt
        ]
        [ H.button
            [ HA.class origin
            , HA.class "cursor-pointer transform p-4 peer"
            , HA.class "transition-transform-opacity"
            , HA.class "flex items-center justify-center"
            , HA.class <| xoExtraClasses isX
            , HE.onClick msg
            ] [svgIcon]
        , H.div
            [ HA.class duration
            , HA.class "fixed top-0 inset-x-0 z-10 -mt-12 sm:-mt-4 md:-mt-24"
            , HA.class "transition-opacity opacity-0 peer-hover:opacity-100"
            , HA.class "pt-8 sm:pt-4 md:pt-8 select-none cursor-default"
            , HA.class "pl-8 sm:pl-0 text-left sm:text-center"
            , HA.class "text-md sm:text-xl md:text-2xl"
            ] [H.text label]
        ]
      -- }}}
    (prompt, xoExtraClasses, containerClasses) =
      -- {{{
      let
        commonContainerClasses = "sm:pr-8 md:pr-16"
      in
      case landingPrompt of
        NoPrompt ->
          -- {{{
          ( formMaker []
          , always <|
                 "opacity-50 hover:opacity-100"
              ++ " hover:scale-100"
              ++ " sm:hover:scale-125"
          , ""
          )
          -- }}}
        NewGamePrompt ->
          -- {{{
          ( formMaker
              [ gamerTagInput ()
              , viewFormButton colorScheme "Create"
              ]
          , \isX ->
              if isX then
                selectedClasses
              else
                disabledClasses
          , commonContainerClasses
          )
          -- }}}
        JoinGamePrompt ->
          -- {{{
          ( formMaker
              [ gamerTagInput ()
              , viewInput
                  colorScheme
                  "gameCode"
                  "game code"
                  gameCode
                  UpdateGameCode
              , viewFormButton colorScheme "Join"
              ]
          , \isX ->
              if isX then
                disabledClasses
              else
                selectedClasses
          , commonContainerClasses
          )
          -- }}}
      -- }}}
    separator =
      -- {{{
      H.div
        [ HA.class "rounded-full shrink-0 select-none"
        , HA.class "w-32 sm:w-0.5 h-0.5 sm:h-32"
        , HA.class "my-4 sm:my-0 mx-0 sm:mx-4 md:mx-8"
        , HA.class colorScheme.grey
        ] []
      -- }}}
  in
  H.div
    [ HA.class "flex items-stretch justify-start"
    , HA.class "sm:items-center sm:justify-center"
    , HA.class "flex-col sm:flex-row z-10 flex-grow h-full"
    -- , HA.style "height" (String.fromInt (pageHeight - 6 * 16) ++ "px")
    , HA.class "transition-transform-opacity"
    , HA.class duration
    , HA.class <|
        if fadingOut then
          "opacity-0 scale-125"
        else
          "opacity-100 scale-100"
    ]
    [ H.div
        [ HA.class "flex items-center justify-center grow sm:grow-0"
        , HA.class "flex-col sm:flex-row z-10 shrink-0"
        , HA.class "px-4 py-0 sm:py-8 transition-padding"
        , HA.class containerClasses
        , HA.class duration
        ]
        [ xoMaker True
        , separator
        , xoMaker False
        ]
    , prompt
    ]
  -- }}}

viewGamePage : ColorScheme -> Bool -> ElmGame -> Html Msg
viewGamePage colorScheme fadingOut (ElmGame info ps) =
  -- {{{
  let
    mXP  = ps.xElmPlayer
    mOP  = ps.oElmPlayer
    wrap =
      -- {{{
      H.div
        [ HA.class "h-full w-full flex flex-col items-center justify-center"
        , HA.class "font-mono flex-grow relative"
        , HA.class colorScheme.txt
        , HA.class "transition-transform-opacity"
        , HA.class duration
        , HA.class <|
            if fadingOut then
              "opacity-0 scale-125"
            else
              "opacity-100 scale-100"
        ]
      -- }}}
    waitingForOpponent _ =
      -- {{{
      wrap
        [ H.div
            [ HA.class "text-center font-xl"
            ] [H.text "Your game's code is:"]
        , H.div
            [ HA.class "text-center font-bold text-2xl"
            ] [H.text info.gameCode]
        , H.div
            [ HA.class "text-center font-xl py-4"
            ] [H.text "Waiting for opponent..."]
        , H.button
            (HE.onClick CloseServer :: buttonAttrs colorScheme)
            [H.text "Close"]
        ]
      -- }}}
    connText isConned =
      H.text <|
           " ("
        ++ ( if isConned then
               "connected"
             else
               "not connected"
           )
        ++ ")"
  in
  case (mXP, mOP) of
    (Nothing, Nothing) ->
      wrap
        [ H.div
            [ HA.class "text-center font-bold text-2xl"
            ] [H.text "Something went wrong..."]
        ]
    (Just _, Nothing) ->
      waitingForOpponent ()
    (Nothing, Just _) ->
      waitingForOpponent ()
    (Just xP, Just oP) ->
      wrap
        [ H.div
            [ HA.class "text-center font-bold text-2xl"
            ]
            [ H.text   xP.elmTag
            , connText xP.isConnected
            ]
        , H.div [HA.class "text-center text-xl"] [H.text "vs"]
        , H.div
            [ HA.class "text-center font-bold text-2xl"
            ]
            [ H.text   oP.elmTag
            , connText oP.isConnected
            ]
        ]
  -- }}}

viewPage : ColorScheme
        -> FadeState
        -> Int
        -> String
        -> String
        -> Page
        -> Html Msg
viewPage colorScheme fadeState pageHeight gamerTag gameCode page =
  -- {{{
  let
    (fadingOut, overflowClass) =
      case fadeState of
        NoFade ->
          (False, "")
        InFade ->
          (False, "overflow-hidden")
        OutFade ->
          (True, "overflow-hidden")
  in
  H.div
    [ HA.class "flex flex-col items-stretch"
    , HA.class "justify-start sm:justify-center"
    , HA.class "relative transition-transform-opacity"
    , HA.class duration
    , HA.style "min-height" (String.fromInt (max 320 pageHeight) ++ "px")
    , HA.class overflowClass
    , HA.class "px-4 sm:px-8 xl:px-16 py-12 sm:py-4 md:py-24"
    ]
    [ H.div
        [ HA.class "w-full h-1/6 max-h-16 sm:max-h-24 opacity-0"
        , HA.class "z-50 absolute inset-x-0 top-0"
        ] []
    , case page of
        LandingPage landingPrompt ->
          viewLandingPage
            colorScheme
            fadingOut
            gamerTag
            gameCode
            landingPrompt
        GamePage game ->
          viewGamePage
            colorScheme
            fadingOut
            game
    ]
  -- }}}

view : Model -> Document Msg
view model =
  -- {{{
  let
    colorScheme = makeColorScheme model.seed
    wrapper comps =
      svgDefs ::
      ( H.div
          [ HA.class "cursor-pointer h-8 w-8 p-2 fixed top-0 right-0 mt-4 mr-4"
          , HA.class "opacity-50 hover:opacity-100 transition-opacity"
          , HA.class "z-100 select-none"
          , HA.class duration
          , HA.class colorScheme.txt
          , HE.onClick (UpdateSeedWith (\x -> x + 1))
          ] [H.text "◑"]
      ) ::
      comps
    fromFadeAndPage fadeState page =
      -- {{{
      wrapper
        [ viewPage
            colorScheme
            fadeState
            model.heightPx
            model.gamerTag
            model.gameCode
            page
        ]
      -- }}}
  in
  { title = model.title
  , body  =
      case model.programView of
        PageView page ->
          fromFadeAndPage NoFade page
        FadingOut _ page ->
          fromFadeAndPage OutFade page
        FadingIn _ page ->
          fromFadeAndPage InFade page
  }
  -- }}}
-- }}}


-- {{{ SUBSCRIPTIONS 
subscriptions : Model -> Sub Msg
subscriptions model =
  -- {{{
  Sub.batch
    [ BE.onResize SetViewportDimensions
    , vesselReceived (HandleVessel << D.decodeValue Vessel.jsonDecVessel)
    ]
  -- }}}

port vesselReceived : (E.Value -> msg) -> Sub msg
-- }}}


-- {{{ UTILS 
giveTimeToMsg : (Int -> Msg) -> Cmd Msg
giveTimeToMsg msg =
  -- {{{
  Task.perform
    msg
    (Task.map Time.posixToMillis Time.now)
  -- }}}


runCmdAfter : Int -> Cmd Msg -> Cmd Msg
runCmdAfter waitMillis cmd =
  -- {{{
  Process.sleep (toFloat waitMillis)
  |> Task.perform (\_ -> RunCmd cmd)
  -- }}}


updateAfter : Int -> Msg -> Cmd Msg
updateAfter waitMillis msg =
  -- {{{
  Process.sleep (toFloat waitMillis)
  |> Task.perform (\_ -> msg)
  -- }}}


getCurrentPage : ProgramView -> Page
getCurrentPage pV =
  -- {{{
  case pV of
    PageView page ->
      page
    FadingOut _ page ->
      page
    FadingIn _ page ->
      page
  -- }}}


httpPost : String -> Http.Body -> Http.Expect Msg -> Cmd Msg
httpPost url body expect =
  -- {{{
  Http.request
    { method  = "POST"
    , headers = []
    , url     = url
    , body    = body
    , expect  = expect
    , timeout = Just timeoutMillis
    , tracker = Nothing
    }
  -- }}}
-- }}}
