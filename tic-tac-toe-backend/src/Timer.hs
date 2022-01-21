{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Timer where


import ClassyPrelude
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay)


performEvery :: MonadIO m => Int -> m a -> m a
performEvery delay action = forever $ do
  liftIO $ threadDelay delay
  action


getCurrentMillis :: IO Int
getCurrentMillis =
  round . (*1000) <$> getPOSIXTime
