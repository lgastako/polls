{-# LANGUAGE NoImplicitPrelude #-}

module PollSource.Ids
  ( UUID
  , genId
  , toText
  ) where

import PollSource.Prelude

import Data.UUID          ( toText )
import Data.UUID.V4       ( nextRandom )

genId :: MonadIO m => m UUID
genId = liftIO nextRandom
