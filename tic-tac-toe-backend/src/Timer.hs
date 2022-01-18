{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}


module Timer where


import ClassyPrelude
import Control.Concurrent (threadDelay)


performEvery :: MonadIO m => Integer -> m a -> m a
performEvery delay action = forever $ do
  liftIO $ threadDelay delay
  action
