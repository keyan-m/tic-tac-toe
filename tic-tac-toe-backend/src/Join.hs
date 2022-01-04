{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}


module Join where


import ClassyPrelude
import Elm.Derive
import Player (Player (..), Players (..), ElmPlayer (..), ElmPlayers (..))
import qualified Player
import Game (Game (..), ElmGame (..))
import qualified Game


-- MODEL
-- {{{
data Result
  = Failed     String
  | Successful Game

data ElmResult
  = ElmFailed     String
  | ElmSuccessful ElmGame
  deriving (Generic, Show)
deriveBoth defaultOptions ''ElmResult

toElm :: Result -> ElmResult
-- {{{
toElm (Failed str) =
  ElmFailed str
toElm (Successful game) =
  ElmSuccessful $ Game.toElm game
-- }}}

fromElm :: ElmResult -> Result
-- {{{
fromElm (ElmFailed str) =
  Failed str
fromElm (ElmSuccessful elmGame) =
  Successful $ Game.fromElm elmGame
-- }}}
-- }}}



-- UTILS
-- {{{
attempt :: String -> ElmPlayer -> Game -> Result
attempt targetCode elmPlayer game@(Game info players) =
  -- {{{
  let
    currCode = Game.gameCode info
  in
  if currCode == targetCode then
    -- {{{
    Successful $ Game info $ players
      { oPlayer = Just $ Player.fromElm elmPlayer
      }
    -- }}}
  else
    -- {{{
    failedInvalidCode
    -- }}}
  -- }}}


failedInvalidCode =
  Failed    "Invalid game code."
elmFailedInvalidCode =
  ElmFailed "Invalid game code."
-- }}}
