{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PollSource.Auth where

import PollSource.Prelude

import PollSource.Types
import Servant.API.BasicAuth ( BasicAuthData( BasicAuthData ) )
import Servant.Server        ( BasicAuthCheck( BasicAuthCheck )
                             , BasicAuthResult( Authorized
                                              , Unauthorized
                                              )
                             , Context( (:.)
                                      , EmptyContext
                                      )
                             )

authCheck :: BasicAuthCheck Session
authCheck = BasicAuthCheck (return . chk)
  where
    chk (BasicAuthData username password)
      | password == "demo" = Authorized . Session . Email . decodeUtf8 $ username
      | otherwise          = Unauthorized

basicAuthServerContext :: Context (BasicAuthCheck Session ': '[])
basicAuthServerContext = authCheck :. EmptyContext
