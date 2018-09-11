{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module PollSource.Handlers
  ( addChoice
  , createPoll
  , editChoice
  , editExpiration
  , inviteUser
  , publishPoll
  , removeChoice
  , selectChoice
  , uninviteUser
  ) where

import PollSource.Prelude

import PollSource.Commands
import PollSource.Ids
import PollSource.Projections.FullPoll
import PollSource.Time
import PollSource.Types

addChoice :: Session -> PollId -> Choice -> AppM AddedChoice
addChoice session pid c = genId >>= \id -> unsafeFromRight
  <$> speculate session DoAddChoice (AddedChoice pid c (ChoiceId id))

createPoll :: Session -> NewPoll -> AppM CreatedPoll
createPoll session np = unsafeFromRight
  <$> speculate session DoCreatePoll (fromNewPoll np)
  where
    fromNewPoll :: NewPoll -> CreatedPoll
    fromNewPoll = CreatedPoll
      <$> (^.expiration)
      <*> (^.pollId)
      <*> (^.question)
      <*> pure (session ^. currentUser)

editChoice :: Session -> PollId -> ChoiceId -> Choice -> AppM EditedChoice
editChoice session pid cid c = unsafeFromRight
  <$> speculate session DoEditChoice (EditedChoice c cid pid)
  -- TODO: confirm pid and cid match

editExpiration :: Session -> PollId -> Expiration -> AppM EditedExpiration
editExpiration session pid exp' = unsafeFromRight
  <$> speculate session DoEditExpiration (EditedExpiration exp' pid)

inviteUser :: Session -> PollId -> Email -> AppM InvitedUser
inviteUser session pid em = unsafeFromRight
  <$> speculate session DoInviteUser (InvitedUser pid em)

publishPoll :: Session -> PollId -> AppM PublishedPoll
publishPoll session pid = liftIO now >>= \at -> unsafeFromRight
  <$> speculate session DoPublishPoll (PublishedPoll pid at)

removeChoice :: Session -> PollId -> ChoiceId -> AppM RemovedChoice
removeChoice session pid cid = unsafeFromRight
  <$> speculate session DoRemoveChoice (RemovedChoice cid pid)

selectChoice :: Session -> PollId -> ChoiceId -> AppM SelectedChoice
selectChoice session pid cid = unsafeFromRight
  <$> speculate session DoSelectChoice (SelectedChoice cid (session ^. currentUser) pid)

uninviteUser :: Session -> PollId -> Email -> AppM UninvitedUser
uninviteUser session pid em = unsafeFromRight
  <$> speculate session DoUninviteUser (UninvitedUser pid em)
