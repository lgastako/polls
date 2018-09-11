{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module PollSource.Queries
  ( getPoll
  , getPollSecure
  ) where

import           PollSource.Prelude
import qualified Streaming.Prelude               as S

import           Dispenser
import           Dispenser.Server                     ( PgConnection )
import           PollSource.Projections.FullPoll
import           PollSource.Streams
import           PollSource.Types

getPoll :: PollId -> AppM (Maybe Poll)
getPoll pid = runProjection (pollSource pid) fullPoll pid

getPollSecure :: Email -> PollId -> AppM (Maybe Poll)
getPollSecure requester pid = getPoll pid >>= \case
  Nothing -> return Nothing
  Just p
    | authorizedView requester p -> return (Just p)
    | otherwise                  -> return Nothing
  where
    authorizedView :: Email -> Poll -> Bool
    authorizedView u p = u == p ^. author || u `elem` p ^. invitees

-- XXX: Generalize/extract
runProjection :: HasEvent (Event PollEvent) e
              => StreamSource -> Projection p e s v -> p -> AppM v
runProjection source proj param = do
  conn :: PgConnection PollEvent <- connect defaultPartition =<< view dispenserClient
  runResourceT . adhocValue proj param . S.map (view eventData)
    =<< runResourceT (currentStream conn batchSize source)
  where
    batchSize = BatchSize 100
