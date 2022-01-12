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
data Result
  = Inconclusive
  | Draw
  | XWon
  | OWon


data Mark
  = X
  | O
  deriving (Generic, Eq, Show)
deriveBoth defaultOptions ''Mark

markFromBool :: Bool -> Mark
markFromBool isX = if isX then X else O

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

playgroundsRow0 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsRow0 =
  -- {{{
  tripletFromAccessor slot00 slot01 slot02
  -- }}}
playgroundsRow1 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsRow1 =
  -- {{{
  tripletFromAccessor slot10 slot11 slot12
  -- }}}
playgroundsRow2 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsRow2 =
  -- {{{
  tripletFromAccessor slot20 slot21 slot22
  -- }}}
playgroundsCol0 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsCol0 =
  -- {{{
  tripletFromAccessor
    slot00
    slot10
    slot20
  -- }}}
playgroundsCol1 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsCol1 =
  -- {{{
  tripletFromAccessor
    slot01
    slot11
    slot21
  -- }}}
playgroundsCol2 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsCol2 =
  -- {{{
  tripletFromAccessor
    slot02
    slot12
    slot22
  -- }}}
playgroundsDia0 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsDia0 =
  -- {{{
  tripletFromAccessor
    slot00
    slot11
    slot22
  -- }}}
playgroundsDia1 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsDia1 =
  -- {{{
  tripletFromAccessor
    slot02
    slot11
    slot20
  -- }}}

findResultFromTriplet :: (Maybe Mark, Maybe Mark, Maybe Mark) -> Result
findResultFromTriplet triplet =
  -- {{{
  case triplet of
    (Just X, Just X, Just X) ->
      XWon
    (Just O, Just O, Just O) ->
      OWon
    (Just _, Just _, Just _) ->
      Draw
    _ ->
      Inconclusive
  -- }}}

combResults :: Result -> Result -> Result
combResults res0 Inconclusive = res0
combResults res0 Draw         = res0
combResults _    res1         = res1
-- }}}

playgroundsResult :: Playground -> Result
playgroundsResult pg =
  -- {{{
  let
    fromFn fn = pg & fn & findResultFromTriplet
  in
    combResults (fromFn playgroundsRow0)
  $ combResults (fromFn playgroundsRow1)
  $ combResults (fromFn playgroundsRow2)
  $ combResults (fromFn playgroundsCol0)
  $ combResults (fromFn playgroundsCol1)
  $ combResults (fromFn playgroundsCol2)
  $ combResults (fromFn playgroundsDia0)
  $ fromFn playgroundsDia1
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


newMoveBy :: String
          -> (Int, Int)
          -> Game
          -> Maybe (Game, Bool, WS.Connection, WS.Connection)
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
            return
              ( Game (info {playground = newPG}) players
              , markBool
              , xConn
              , oConn
              )
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


