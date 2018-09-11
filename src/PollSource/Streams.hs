{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PollSource.Streams where

import PollSource.Prelude

import Dispenser
import PollSource.Ids
import PollSource.Types

pollStream :: PollId -> StreamName
pollStream pid = StreamName $ "/poll/" <> (toText . unPollId) pid

pollSource :: PollId -> StreamSource
pollSource = singletonSource . pollStream
