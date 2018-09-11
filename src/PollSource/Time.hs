{-# LANGUAGE NoImplicitPrelude #-}

module PollSource.Time
  ( MonadNow(..)
  , UTCTime
  ) where

import PollSource.Prelude

import Data.Time.Clock

class MonadNow m where
  now :: m UTCTime

instance MonadNow IO where
  now = getCurrentTime
