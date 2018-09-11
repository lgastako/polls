{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module PollSource.Projections.FullPoll
  ( Poll
  , author
  , choices
  , expiration
  , fullPoll
  , invitees
  {- -v- just to avoid warnings -v- -}
  , pollAuthor
  , pollChoices
  , pollExpiration
  , pollInvitees
  , pollPollId
  , pollPublished
  , pollQuestion
  , pollSelections
  {- -^- just to avoid warnings -^- -}
  , pollId
  , published
  , question
  , selections
  ) where

import PollSource.Prelude

import Data.Map.Strict       as Map ( insert )
import Data.Set              as Set ( delete
                                    , insert
                                    )
import Dispenser.Projections
import PollSource.Types

data Poll = Poll
  { pollAuthor     :: Email
  , pollChoices    :: [(ChoiceId, Choice)]
  , pollExpiration :: Expiration
  , pollInvitees   :: Set Email
  , pollPollId     :: PollId
  , pollPublished  :: Maybe UTCTime
  , pollQuestion   :: Question
  , pollSelections :: Map Email ChoiceId
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance ToJSON Poll

makeFields ''Poll

fullPoll :: Projection PollId PollEvent (Maybe Poll) (Maybe Poll)
fullPoll = Projection (\pid -> Fold (f pid) Nothing identity) identity
  where
    f :: PollId -> Maybe Poll -> PollEvent -> Maybe Poll
    f pid st e = case e of

      ChoiceAdded ac -> checkingPid ac (& choices %~ (<> [(ac ^. choiceId, ac ^. choice)]))

      ChoiceEdited ec -> checkingPid ec (& choices %~ map (editPair ec))
        where
          editPair :: EditedChoice -> (ChoiceId, Choice) -> (ChoiceId, Choice)
          editPair ec' ch@(cid, _)
            | ec' ^. choiceId == cid = (cid, ec' ^. choice)
            | otherwise = ch

      ChoiceRemoved rc -> checkingPid rc
        (& choices %~ filter ((/= rc ^. choiceId) . fst))

      ChoiceSelected sc -> checkingPid sc
        (& selections %~ Map.insert (sc ^. email) (sc ^. choiceId))

      ExpirationEdited ee -> checkingPid ee (& expiration .~ (ee ^. expiration))

      PollCreated cp -> case st of
        Just exists -> Just exists -- poll already exists, cannot overwrite
        Nothing -> Just $ xform cp
          where
            xform :: CreatedPoll -> Poll
            xform = Poll <$> (^.email) <*> mempty <*> (^.expiration) <*> mempty
              <*> (^.pollId) <*> return Nothing <*> (^.question) <*> mempty

      PollPublished pp -> checkingPid pp g
        where
          g x = case x ^. published of
                  Nothing -> x & published .~ Just (pp ^. publishedAt)
                  Just _  -> x -- already published

      UserInvited ui -> checkingPid ui (& invitees %~ Set.insert (ui ^. email))

      UserUninvited uu -> checkingPid uu (& invitees %~ Set.delete (uu ^. email))

      where
        checkingPid :: HasPollId a PollId => a -> (Poll -> Poll) -> Maybe Poll
        checkingPid payload g
          | pid == payload ^. pollId = g <$> st
          | otherwise = st
