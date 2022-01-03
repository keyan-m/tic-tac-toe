{-# LANGUAGE NoImplicitPrelude #-}

module RandomCode (uniqueRandom6Chars) where

import ClassyPrelude
import Data.Function ((&))
import Data.Char (chr, isAlpha)
import System.Random

randomChar :: MonadIO m => m Char
randomChar =
  fmap
    ( \x ->
          genWord8 (mkStdGen x)
        & fst
        & fromIntegral
        & (\y -> y * 25 / 255 + 65)
        & round
        & chr
    )
    randomIO

randomString :: MonadIO m => Int -> m String
randomString charCount = replicateM charCount randomChar

uniqueRandom6Chars :: MonadIO m => [String] -> m String
uniqueRandom6Chars taken = do
  attempt <- randomString 6
  if ClassyPrelude.elem attempt taken then
    uniqueRandom6Chars taken
  else
    return (filter isAlpha attempt)
