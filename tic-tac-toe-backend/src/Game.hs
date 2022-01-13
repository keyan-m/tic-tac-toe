-- EXTENSIONS
-- {{{
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}
-- }}}


-- MODULE
-- {{{
module Game
  ( Game (..)
  , ElmGame (..)
  , NewMoveOutcome (..)
  , Direction (..)
  , GameResult (..)
  , fromElm
  , toElm
  , Info (..)
  , Mark (..)
  , Playground
  , getCode
  , new
  , kickOff
  , join
  , newMoveBy
  ) where
-- }}}


-- IMPORTS
-- {{{
import ClassyPrelude
import Data.Function ((&))
import Elm.Derive
import Player
  ( Player (..)
  , ElmPlayer (..)
  , Players (..)
  , ElmPlayers (..)
  )
import qualified Player
import qualified Network.WebSockets as WS
-- }}}


-- MODEL
-- {{{

data Mark
  = X
  | O
  deriving (Generic, Eq, Show)
deriveBoth defaultOptions ''Mark

markFromBool :: Bool -> Mark
markFromBool isX = if isX then X else O


data Direction
  = Row0
  | Row1
  | Row2
  | Col0
  | Col1
  | Col2
  | Dia0
  | Dia1
  deriving (Generic, Eq, Show)
deriveBoth defaultOptions ''Direction


data MarkTriplet = MarkTriplet
  { dir   :: Direction
  , marks :: (Maybe Mark, Maybe Mark, Maybe Mark)
  }

data GameResult
  = Draw
  | XWon Direction
  | OWon Direction
  deriving (Generic, Eq, Show)
deriveBoth defaultOptions ''GameResult


-- PLAYGROUND (3 x 3) TODO: add more playground sizes.
-- {{{
data Playground = Playground
  { slot00 :: Maybe (Int, Mark)
  , slot01 :: Maybe (Int, Mark)
  , slot02 :: Maybe (Int, Mark)
  , slot10 :: Maybe (Int, Mark)
  , slot11 :: Maybe (Int, Mark)
  , slot12 :: Maybe (Int, Mark)
  , slot20 :: Maybe (Int, Mark)
  , slot21 :: Maybe (Int, Mark)
  , slot22 :: Maybe (Int, Mark)
  } deriving (Generic, Show)
deriveBoth defaultOptions ''Playground
-- {{{ PLAYGROUND UTILS 
emptyPlayground = Playground
  -- {{{
  { slot00 = Nothing
  , slot01 = Nothing
  , slot02 = Nothing
  , slot10 = Nothing
  , slot11 = Nothing
  , slot12 = Nothing
  , slot20 = Nothing
  , slot21 = Nothing
  , slot22 = Nothing
  }
  -- }}}

playgroundToList :: Playground -> [(Int, Mark)]
playgroundToList
  Playground
    { slot00 = slot00 , slot01 = slot01 , slot02 = slot02
    , slot10 = slot10 , slot11 = slot11 , slot12 = slot12
    , slot20 = slot20 , slot21 = slot21 , slot22 = slot22
    } =
  -- {{{
  [ slot00 , slot01 , slot02
  , slot10 , slot11 , slot12
  , slot20 , slot21 , slot22
  ]
  & filter isJust
  & sequence
  & fromMaybe []
  -- }}}

setMarkAt :: (Int, Mark) -> (Int, Int) -> Playground -> Maybe Playground
setMarkAt mark coords pg =
  -- {{{
  let
    mMark = Just mark
    ifIsFreeThen slot newPG =
      case slot pg of
        Nothing -> Just newPG
        Just _  -> Nothing
  in
  case coords of
    (0, 0) -> ifIsFreeThen slot00 $ pg {slot00 = mMark}
    (0, 1) -> ifIsFreeThen slot01 $ pg {slot01 = mMark}
    (0, 2) -> ifIsFreeThen slot02 $ pg {slot02 = mMark}
    (1, 0) -> ifIsFreeThen slot10 $ pg {slot10 = mMark}
    (1, 1) -> ifIsFreeThen slot11 $ pg {slot11 = mMark}
    (1, 2) -> ifIsFreeThen slot12 $ pg {slot12 = mMark}
    (2, 0) -> ifIsFreeThen slot20 $ pg {slot20 = mMark}
    (2, 1) -> ifIsFreeThen slot21 $ pg {slot21 = mMark}
    (2, 2) -> ifIsFreeThen slot22 $ pg {slot22 = mMark}
    _      -> Nothing
  -- }}}


getLastMove :: Playground -> Maybe (Int, Mark)
getLastMove pg =
  -- {{{
    playgroundToList pg
  & maximumByMay (\(ind0, _) (ind1, _) -> compare ind0 ind1)
  -- }}}

tripletFromAccessor acc1 acc2 acc3 pg =
  (snd <$> acc1 pg, snd <$> acc2 pg, snd <$> acc3 pg)

playgroundToMarkTriplet :: Direction -> Playground -> MarkTriplet
playgroundToMarkTriplet dir =
  -- {{{
  MarkTriplet dir .
    case dir of
      Row0 ->
        tripletFromAccessor slot00 slot01 slot02
      Row1 ->
        tripletFromAccessor slot10 slot11 slot12
      Row2 ->
        tripletFromAccessor slot20 slot21 slot22
      Col0 ->
        tripletFromAccessor
          slot00
          slot10
          slot20
      Col1 ->
        tripletFromAccessor
          slot01
          slot11
          slot21
      Col2 ->
        tripletFromAccessor
          slot02
          slot12
          slot22
      Dia0 ->
        tripletFromAccessor
          slot00
          slot11
          slot22
      Dia1 ->
        tripletFromAccessor
          slot02
          slot11
          slot20
  -- }}}

findResultFromMarkTriplet :: MarkTriplet -> Maybe GameResult
findResultFromMarkTriplet (MarkTriplet dir triplet) =
  -- {{{
  case triplet of
    (Just X, Just X, Just X) ->
      Just $ XWon dir
    (Just O, Just O, Just O) ->
      Just $ OWon dir
    (Just _, Just _, Just _) ->
      Just Draw
    _ ->
      Nothing
  -- }}}

combResults :: Maybe GameResult -> Maybe GameResult -> Maybe GameResult
combResults res@(Just (XWon dir)) _                     = res
combResults _                     res@(Just (XWon dir)) = res
combResults res@(Just (OWon dir)) _                     = res
combResults _                     res@(Just (OWon dir)) = res
combResults Nothing               _                     = Nothing
combResults _                     Nothing               = Nothing
combResults _                     _                     = Just Draw
-- }}}

playgroundsResult :: Playground -> Maybe GameResult
playgroundsResult pg =
  -- {{{
  let
    fromFn fn = pg & fn & findResultFromMarkTriplet
  in do
    combResults (fromFn $ playgroundToMarkTriplet Row0)
  $ combResults (fromFn $ playgroundToMarkTriplet Row1)
  $ combResults (fromFn $ playgroundToMarkTriplet Row2)
  $ combResults (fromFn $ playgroundToMarkTriplet Col0)
  $ combResults (fromFn $ playgroundToMarkTriplet Col1)
  $ combResults (fromFn $ playgroundToMarkTriplet Col2)
  $ combResults (fromFn $ playgroundToMarkTriplet Dia0)
                (fromFn $ playgroundToMarkTriplet Dia1)
  -- }}}
-- }}}


-- GAME
-- {{{
data Info = Info
  { gameCode   :: String
  , xStarted   :: Bool
  , playground :: Playground
  } deriving (Generic, Show)
deriveBoth defaultOptions ''Info

data Game = Game Info Players

getCode :: Game -> String
getCode (Game info _) =
  -- {{{
  gameCode info
  -- }}}


new :: String -> String -> Game
new newCode xPTag =
  -- {{{
  Game
    ( Info
        { gameCode = newCode
        , xStarted = True
        , playground = emptyPlayground
        }
    )
    ( Players
        { Player.xPlayer = Just $ Player xPTag Nothing
        , Player.oPlayer = Nothing
        }
    )
  -- }}}


kickOff :: String -> Bool -> Player -> Player -> Game
kickOff code xToStart xP oP =
  -- {{{
  Game
    ( Info
        { gameCode   = code
        , xStarted   = xToStart
        , playground = emptyPlayground
        }
    )
    ( Players
        { Player.xPlayer = Just xP
        , Player.oPlayer = Just oP
        }
    )
  -- }}}


data NewMoveOutcome = NewMoveOutcome
  { updatedGame     :: Game
  , moveWasByX      :: Bool
  , xPlayersConn    :: WS.Connection
  , oPlayersConn    :: WS.Connection
  , resultAfterMove :: Maybe GameResult
  }


newMoveBy :: String
          -> (Int, Int)
          -> Game
          -- -> Maybe (Game, Bool, WS.Connection, WS.Connection)
          -> Maybe NewMoveOutcome
newMoveBy moveBy
          coords
          g@(Game info players@Players {xPlayer = mXP, oPlayer = mOP}) =
  -- {{{
  case (mXP, mOP) of
    (Just (Player xTag (Just xConn)), Just (Player oTag (Just oConn))) -> do
      -- {{{
      moveByX <- if moveBy == xTag then
                   Just True
                 else if moveBy == oTag then
                   Just False
                 else
                   Nothing
      let fromCountAndBool newCount markBool = do
            newPG <- setMarkAt (newCount, markFromBool markBool)
                               coords
                               (playground info)
            return $ NewMoveOutcome
              { updatedGame     = Game (info {playground = newPG}) players
              , moveWasByX      = markBool
              , xPlayersConn    = xConn
              , oPlayersConn    = oConn
              , resultAfterMove = playgroundsResult newPG
              }
      case getLastMove $ playground info of
        Nothing ->
          -- {{{
          if xStarted info == moveByX then
            fromCountAndBool 0 moveByX
          else
            Nothing
          -- }}}
        Just (count, lastMark) ->
          -- {{{
          if markFromBool moveByX == lastMark then
            Nothing
          else if count >= 8 then
            Nothing
          else
            fromCountAndBool (count + 1) moveByX
          -- }}}
      -- }}}
    _ ->
      -- {{{
      Nothing
      -- }}}
  -- }}}
-- }}}


-- ELMGAME
-- {{{
data ElmGame = ElmGame Info ElmPlayers deriving (Generic, Show)
deriveBoth defaultOptions ''ElmGame

fromElm :: ElmGame -> Game
fromElm (ElmGame info players) = Game info (Player.fromElms players)

toElm :: Game -> ElmGame
toElm (Game info players) = ElmGame info (Player.toElms players)
-- }}}


-- }}}


