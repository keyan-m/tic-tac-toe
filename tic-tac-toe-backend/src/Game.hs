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
  ( Playground
      { slot00 = slot00 , slot01 = slot01 , slot02 = slot02
      , slot10 = slot10 , slot11 = slot11 , slot12 = slot12
      , slot20 = slot20 , slot21 = slot21 , slot22 = slot22
      }
  ) =
  -- {{{
  [ slot00 , slot01 , slot02
  , slot10 , slot11 , slot12
  , slot20 , slot21 , slot22
  ]
  & filter isJust
  & sequence
  & fromMaybe []
  -- }}}

setMarkAt :: (Int, Mark) -> (Int, Int) -> Playground -> Playground
setMarkAt mark coords pg =
  case coords of
    (0, 0) -> pg {slot00 = Just mark}
    (0, 1) -> pg {slot01 = Just mark}
    (0, 2) -> pg {slot02 = Just mark}
    (1, 0) -> pg {slot10 = Just mark}
    (1, 1) -> pg {slot11 = Just mark}
    (1, 2) -> pg {slot12 = Just mark}
    (2, 0) -> pg {slot20 = Just mark}
    (2, 1) -> pg {slot21 = Just mark}
    (2, 2) -> pg {slot22 = Just mark}
    _      -> pg

getLastMove :: Playground -> Maybe (Int, Mark)
getLastMove pg =
  -- {{{
    playgroundToList pg
  & maximumByMay (\(ind0, _) (ind1, _) -> compare ind0 ind1)
  -- }}}

playgroundsRow0 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsRow0 pg =
  -- {{{
  ( fmap snd $ slot00 pg , fmap snd $ slot01 pg , fmap snd $ slot02 pg
  )
  -- }}}
playgroundsRow1 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsRow1 pg =
  -- {{{
  ( fmap snd $ slot10 pg , fmap snd $ slot11 pg , fmap snd $ slot12 pg
  )
  -- }}}
playgroundsRow2 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsRow2 pg =
  -- {{{
  ( fmap snd $ slot20 pg , fmap snd $ slot21 pg , fmap snd $ slot22 pg
  )
  -- }}}
playgroundsCol0 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsCol0 pg =
  -- {{{
  ( fmap snd $ slot00 pg
  , fmap snd $ slot10 pg
  , fmap snd $ slot20 pg
  )
  -- }}}
playgroundsCol1 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsCol1 pg =
  -- {{{
  ( fmap snd $ slot01 pg
  , fmap snd $ slot11 pg
  , fmap snd $ slot21 pg
  )
  -- }}}
playgroundsCol2 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsCol2 pg =
  -- {{{
  ( fmap snd $ slot02 pg
  , fmap snd $ slot12 pg
  , fmap snd $ slot22 pg
  )
  -- }}}
playgroundsDia0 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsDia0 pg =
  -- {{{
  ( fmap snd $ slot00 pg
  ,                         fmap snd $ slot11 pg
  ,                                                fmap snd $ slot22 pg
  )
  -- }}}
playgroundsDia1 :: Playground -> (Maybe Mark, Maybe Mark, Maybe Mark)
playgroundsDia1 pg =
  -- {{{
  (                                               fmap snd $ slot02 pg
  ,                        fmap snd $ slot11 pg
  , fmap snd $ slot20 pg
  )
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


new :: String -> ElmPlayer -> Game
new newCode elmPlayer =
  -- {{{
  Game
    ( Info
        { gameCode = newCode
        , xStarted = True
        , playground = emptyPlayground
        }
    )
    ( Players
        { Player.xPlayer = Just $ Player.fromElm elmPlayer
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


