{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module PollSource.Types where

import PollSource.Prelude

import Data.Serialize      ( Serialize )
import Data.Serialize.Text ()
import Dispenser.Server
import Servant.Server      ( Handler )

type AppM = ReaderT AppContext Handler

newtype AppContext = AppContext
  { appContextDispenserClient :: PgClient PollEvent
  }

newtype Choice = Choice { unChoice :: Text }
  deriving (Eq, Data, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance EventData Choice

newtype ChoiceId = ChoiceId { unChoiceId :: UUID }
  deriving (Eq, Data, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance EventData ChoiceId

instance FromHttpApiData ChoiceId where
  parseQueryParam = ChoiceId <<$>> parseQueryParam

newtype Expiration = Expiration { unExpiration :: Day }
  deriving (Eq, Data, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance EventData Expiration

newtype PollId = PollId { unPollId :: UUID }
  deriving (Eq, Data, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance EventData PollId

instance FromHttpApiData PollId where
  parseQueryParam = PollId <<$>> parseQueryParam

newtype Question = Question { unQuestion :: Text }
  deriving (Eq, Data, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance EventData Question

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Data, FromJSON, Generic, Ord, Read, Serialize, Show, ToJSON)

instance EventData Email


instance FromHttpApiData Email where
  parseQueryParam = Email <<$>> parseQueryParam

instance ToJSONKey Email

newtype Session = Session
  { sessionCurrentUser :: Email
  }

-- ================================================================
--  Events
-- ================================================================

data PollEvent
  = ChoiceAdded      AddedChoice
  | ChoiceEdited     EditedChoice
  | ChoiceRemoved    RemovedChoice
  | ChoiceSelected   SelectedChoice
  | ExpirationEdited EditedExpiration
  | UserInvited      InvitedUser
  | UserUninvited    UninvitedUser
  | PollCreated      CreatedPoll
  | PollPublished    PublishedPoll
  deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  PollEvent
instance ToJSON    PollEvent
instance EventData PollEvent

data AddedChoice = AddedChoice
  { addedChoicePollId   :: PollId
  , addedChoiceChoice   :: Choice
  , addedChoiceChoiceId :: ChoiceId
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  AddedChoice
instance ToJSON    AddedChoice
instance EventData AddedChoice

data CreatedPoll = CreatedPoll
  { createdPollExpiration :: Expiration
  , createdPollPollId     :: PollId
  , createdPollQuestion   :: Question
  , createdPollEmail      :: Email
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  CreatedPoll
instance ToJSON    CreatedPoll
instance EventData CreatedPoll

data EditedChoice = EditedChoice
 { editedChoiceChoice   :: Choice
 , editedChoiceChoiceId :: ChoiceId
 , editedChoicePollId   :: PollId
 } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  EditedChoice
instance ToJSON    EditedChoice
instance EventData EditedChoice

data EditedExpiration = EditedExpiration
  { editedExpirationExpiration :: Expiration
  , editedExpirationPollId     :: PollId
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  EditedExpiration
instance ToJSON    EditedExpiration
instance EventData EditedExpiration

data InvitedUser = InvitedUser
  { invitedUserPollId :: PollId
  , invitedUserEmail  :: Email
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  InvitedUser
instance ToJSON    InvitedUser
instance EventData InvitedUser

data NewPoll = NewPoll
  { newPollExpiration :: Expiration
  , newPollPollId     :: PollId
  , newPollQuestion   :: Question
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  NewPoll
instance ToJSON    NewPoll
instance EventData NewPoll  -- Not strictly speaking necessary, but... eh.

data PublishedPoll = PublishedPoll
  { publishedPollPollId      :: PollId
  , publishedPollPublishedAt :: UTCTime
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  PublishedPoll
instance ToJSON    PublishedPoll
instance EventData PublishedPoll

data RemovedChoice = RemovedChoice
  { removedChoiceChoiceId :: ChoiceId
  , removedChoicePollId   :: PollId
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  RemovedChoice
instance ToJSON    RemovedChoice
instance EventData RemovedChoice

data SelectedChoice = SelectedChoice
  { selectedChoiceChoiceId :: ChoiceId
  , selectedChoiceEmail    :: Email
  , selectedChoicePollId   :: PollId
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  SelectedChoice
instance ToJSON    SelectedChoice
instance EventData SelectedChoice

data UninvitedUser = UninvitedUser
  { uninvitedUserPollId :: PollId
  , uninvitedUserEmail  :: Email
  } deriving (Eq, Data, Generic, Ord, Read, Show)

instance FromJSON  UninvitedUser
instance ToJSON    UninvitedUser
instance EventData UninvitedUser

makeFields ''AddedChoice
makeFields ''AppContext
makeFields ''CreatedPoll
makeFields ''EditedChoice
makeFields ''EditedExpiration
makeFields ''InvitedUser
makeFields ''NewPoll
makeFields ''PublishedPoll
makeFields ''RemovedChoice
makeFields ''SelectedChoice
makeFields ''Session
makeFields ''UninvitedUser
