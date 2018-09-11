{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module PollSource.Commands
  ( Command
  , PollCommand(..)
  , executeCommand
  , speculate
  ) where

import           PollSource.Prelude

import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Dispenser.Server
import           PollSource.Projections.FullPoll
import           PollSource.Queries                     ( getPoll )
import           PollSource.Streams                     ( pollStream )
import           PollSource.Types

-- XXX: generalize/extract to dispenser
--
-- For now all events are posted to the same stream set.  When this is pulled
-- into dispenser it should be re-worked to accomodate posting a batch of
-- events with assorted streams that are nevertheless commited in a single
-- transaction and in the order generated regardless of stream overlap or
-- lackthereof.
class Command c where
  apply      :: c -> AppM (Set StreamName, [PollEvent])
  authorized :: c -> Email -> AppM Bool
  execute    :: c -> AppM (Maybe EventNumber)

data PollCommand
  = DoAddChoice AddedChoice
  | DoEditChoice EditedChoice
  | DoRemoveChoice RemovedChoice
  | DoSelectChoice SelectedChoice
  | DoEditExpiration EditedExpiration
  | DoInviteUser InvitedUser
  | DoUninviteUser UninvitedUser
  | DoCreatePoll CreatedPoll
  | DoPublishPoll PublishedPoll

isOwner :: HasPollId a PollId => Email -> a -> AppM Bool
isOwner user e = getPoll (e ^. pollId) >>= \case
  Just p  -> return $ p ^. author == user
  Nothing -> return False

-- TODO: field size limits
accept :: PollEvent -> AppM Bool
accept (PollCreated _)      = return True -- TODO: expiration in future / validate email
accept (ChoiceAdded e)      = maybe False (not . isPublished) <$> getPoll (e ^. pollId)
accept (ChoiceEdited e)     = (\p -> choiceExists e p && maybe False (not . isPublished) p)
  <$> getPoll (e ^. pollId)
accept (ChoiceRemoved e)    = (\p -> choiceExists e p && maybe False (not . isPublished) p)
  <$> getPoll (e ^. pollId)
accept (ChoiceSelected e)   = getPoll (e ^. pollId) >>= \pm -> return $ choiceExists e pm
  && maybe False (\p -> e ^. email `elem` view invitees p) pm
  && maybe False (\p -> e ^. email `notElem` (Map.keys . view selections) p) pm
accept (ExpirationEdited e) = maybe False (not . isPublished) <$> getPoll (e ^. pollId)
  -- TODO: expiration still in future
accept (UserInvited _)      = return True -- TODO: validate email
accept (UserUninvited e)    = maybe False (\p -> e ^. email `elem` view invitees p)
  <$> getPoll (e ^. pollId)
accept (PollPublished e)    = maybe False (\p -> null $ p ^. published)
  <$> getPoll (e ^. pollId)

isPublished :: Poll -> Bool
isPublished = isJust . view published

choiceExists :: (HasPollId a PollId, HasChoiceId a ChoiceId) => a -> Maybe Poll -> Bool
choiceExists e = maybe False (\p -> e ^. choiceId `elem` map fst (p ^. choices))

genApply :: HasPollId a PollId
         => (a -> PollEvent) -> a -> AppM (Set StreamName, [PollEvent])
genApply constr payload = bool (streams, [e]) mempty <$> accept e
  where
    pid     = view pollId payload
    streams = Set.singleton . pollStream $ pid
    e       = constr payload

instance Command PollCommand where
  apply :: PollCommand -> AppM (Set StreamName, [PollEvent])
  apply (DoCreatePoll cp)     = genApply PollCreated      cp
  apply (DoAddChoice ac)      = genApply ChoiceAdded      ac
  apply (DoEditChoice ec)     = genApply ChoiceEdited     ec
  apply (DoRemoveChoice rc)   = genApply ChoiceRemoved    rc
  apply (DoSelectChoice sc)   = genApply ChoiceSelected   sc
  apply (DoEditExpiration ee) = genApply ExpirationEdited ee
  apply (DoInviteUser iu)     = genApply UserInvited      iu
  apply (DoUninviteUser uu)   = genApply UserUninvited    uu
  apply (DoPublishPoll pp)    = genApply PollPublished    pp

  authorized (DoCreatePoll cp) user    = return $ cp ^. email == user
  authorized (DoAddChoice e) user      = isOwner user e
  authorized (DoEditChoice e) user     = isOwner user e
  authorized (DoRemoveChoice e) user   = isOwner user e
  authorized (DoSelectChoice e) user   = getPoll (e ^. pollId) >>= \case
    Just p  -> return $ e ^. email == user && user `elem` p ^. invitees
    Nothing -> return False
  authorized (DoEditExpiration e) user = isOwner user e
  authorized (DoInviteUser e) user     = isOwner user e
  authorized (DoUninviteUser e) user   = isOwner user e
  authorized (DoPublishPoll e) user    = isOwner user e

  execute cmd = do
    conn :: PgConnection PollEvent <- connect defaultPartition =<< view dispenserClient
    apply cmd >>= \case
      (_, []) -> return Nothing
      (streams, x:xs) -> Just <$> runResourceT (appendEvents conn streams $ fromEvents x xs)
    where
      fromEvents e es = NonEmptyBatch (e :| es)

-- XXX: Return something like CmdResult that is Accepted, Rejected, etc. based
--       on early validation, late validation, etc.
executeCommand :: Command c => Email -> c -> AppM (Either Text (Maybe EventNumber))
executeCommand user cmd = do
  allowed <- authorized cmd user
  if allowed
    then Right <$> execute cmd
    else Left  <$> return "not authorized to execute command"

-- XXX: Command c => ish
speculate :: Session -> (a -> PollCommand) -> a -> AppM (Either Text a)
speculate session constr payload = const payload
  <<$>> executeCommand (session ^. currentUser) cmd
  where
    cmd = constr payload
