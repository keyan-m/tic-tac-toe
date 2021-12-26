module Main where

import ClassyPrelude
import Api
import qualified Bridge

main :: IO ()
main = do
  Bridge.run
  startApp
