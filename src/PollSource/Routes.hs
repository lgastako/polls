{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module PollSource.Routes where

import PollSource.Prelude

import PollSource.Projections.FullPoll ( Poll )
import PollSource.Types
import Servant

-- TODO: to be continued
-- https://haskell-servant.readthedocs.io/en/v0.11.0.1/tutorial/Authentication.html

-- GET /polls/:pollId
type GetPollR
  = "polls" :> Capture "pollId" PollId
  :> Get '[JSON] (Maybe Poll)

-- POST /polls
type CreatePollR
  -- :> AppAuth -- TODO: GRRR
  = "polls"
  :> ReqBody '[JSON] NewPoll
  :> Post    '[JSON] CreatedPoll

-- DELETE /polls/:pollId/choices/:choiceId
type RemoveChoiceR
  =  "polls"   :> Capture "pollId"   PollId
  :> "choices" :> Capture "choiceId" ChoiceId
  :> Delete '[JSON] RemovedChoice

-- POST /polls/:pollId/choices/:choiceId/select
type SelectChoiceR
  =  "polls"   :> Capture "pollId"   PollId
  :> "choices" :> Capture "choiceId" ChoiceId
  :> "select"
  :> Post '[JSON] SelectedChoice

-- POST /polls/:pollId/choices/:choiceId
type EditChoiceR
  =  "polls"   :> Capture "pollId"   PollId
  :> "choices" :> Capture "choiceId" ChoiceId
  :> ReqBody '[JSON] Choice
  :> Post    '[JSON] EditedChoice

-- DELETE /polls/:pollId/invitations/:email
type UninviteUserR
  =  "polls"       :> Capture "pollId" PollId
  :> "invitations" :> Capture "email"  Email
  :> Delete '[JSON] UninvitedUser

-- POST /polls/:pollId/invitations
type InviteUserR
  =  "polls" :>  Capture "pollId" PollId :> "invitations"
  :> ReqBody '[JSON] Email
  :> Post    '[JSON] InvitedUser

-- POST /polls/:pollId/expiration
type EditExpirationR
  =  "polls" :>  Capture "pollId" PollId :> "expiration"
  :> ReqBody '[JSON] Expiration
  :> Post    '[JSON] EditedExpiration

-- POST /polls/:pollId/choices
type AddChoiceR
  =  "polls" :> Capture "pollId" PollId :> "choices"
  :> ReqBody '[JSON] Choice
  :> Post    '[JSON] AddedChoice

-- POST /polls/:pollId/publish
type PublishPollR
  =  "polls" :> Capture "pollId" PollId :> "publish"
  :> Post '[JSON] PublishedPoll
