{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module PollSource.Server
  ( servePolls
  , writeJs
  , writeRuby
  ) where

import PollSource.Prelude

import Dispenser.Server                     ( PgConnection
                                            , connect
                                            , ensureExists
                                            , new
                                            )
import Network.Wai.Handler.Warp             ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import PollSource.Auth
import PollSource.Handlers
import PollSource.Queries
import PollSource.Routes
import PollSource.Types
import Servant
import Servant.JS                           ( defCommonGeneratorOptions
                                            , jsForAPI
                                            , vanillaJSWith
                                            )
import Servant.Ruby                         ( NameSpace( NameSpace )
                                            , ruby
                                            )

type Protected = BasicAuth "poll-realm" Session

type PollApi = PublicApi :<|> Protected :> PrivateApi

type PublicApi = Get '[JSON] ()  -- TODO: Html w/ home page

type PrivateApi = CreatePollR
             :<|> GetPollR
             :<|> RemoveChoiceR
             :<|> SelectChoiceR
             :<|> EditChoiceR
             :<|> UninviteUserR
             :<|> InviteUserR
             :<|> EditExpirationR
             :<|> AddChoiceR
             :<|> PublishPollR

sessionServer :: Session -> ServerT PrivateApi AppM
sessionServer session = createPoll     session
                   :<|> getPollSecure  (session ^. currentUser)
                   :<|> removeChoice   session
                   :<|> selectChoice   session
                   :<|> editChoice     session
                   :<|> uninviteUser   session
                   :<|> inviteUser     session
                   :<|> editExpiration session
                   :<|> addChoice      session
                   :<|> publishPoll    session

mkPrivateServer :: AppContext -> Session -> Server PrivateApi
mkPrivateServer ctx = enter (runReaderTNat ctx :: AppM :~> Handler) . sessionServer

mkApp :: AppContext -> Application
mkApp = logStdoutDev . serveWithContext pollApi basicAuthServerContext . mkProtectedServer

mkProtectedServer :: AppContext -> Server PollApi
mkProtectedServer ctx = publicAPIHandler :<|> privateApiHandler
  where
    privateApiHandler :: Session -> Server PrivateApi
    privateApiHandler = mkPrivateServer ctx

    publicAPIHandler :: Server PublicApi
    publicAPIHandler = return ()

pollApi :: Proxy PollApi
pollApi = Proxy

privateApi :: Proxy PrivateApi
privateApi = Proxy

servePolls :: Int -> IO ()
servePolls port = do
  putLn $ "PollSource Server listening on http://localhost:" <> show port
  dispClient <- new poolMax url'
  conn :: PgConnection PollEvent <- connect defaultPartition dispClient
  ensureExists conn
  run port . mkApp $ AppContext dispClient
  where
    -- TODO: get url and pool max from env with defaults
    url'    = "postgres://polls@localhost:5432/polls"
    poolMax = 5

writeJs :: MonadIO m => FilePath -> m ()
writeJs path = liftIO $ writeFile path (fixJs . jsForAPI privateApi $ vanillaJSWith genOpts)
  where
    fixJs   = identity
    genOpts = defCommonGeneratorOptions
--    genOpts = defCommonGeneratorOptions { moduleName = "module.exports" }

writeRuby :: MonadIO m => FilePath -> m ()
writeRuby path = liftIO . writeFile path $ ruby (NameSpace [] "Polls") privateApi
