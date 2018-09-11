{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PollSource.Prelude
  ( module X
  , defaultPartition
  , putLn
  , unsafeFromRight
  ) where

import Control.Foldl      as X ( Fold( Fold ) )
import Control.Lens       as X ( (%~)
                               , (.~)
                               , (^.)
                               , makeFields
                               , over
                               , view
                               )
import Data.Aeson         as X ( FromJSON
                               , ToJSON
                               , ToJSONKey
                               )
import Data.Data          as X ( Data )
import Data.Time.Calendar as X ( Day )
import Data.Time.Clock    as X ( UTCTime )
import Data.UUID          as X ( UUID )
import Dispenser          as X ( EventData
                               , PartitionName(..)
                               )
import Protolude          as X
import Safe               as X ( readMay )
import Streaming          as X ( runResourceT )
import Web.HttpApiData    as X ( FromHttpApiData
                               , parseQueryParam
                               )

defaultPartition :: PartitionName
defaultPartition = PartitionName "poll_events"

putLn :: MonadIO m => Text -> m ()
putLn = putStrLn

unsafeFromRight :: Show a => Either a b -> b
unsafeFromRight (Left a)  = panic $ "unsafeFromRight called on: " <> show a
unsafeFromRight (Right b) = b
