{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Auth where

import Control.Monad.Trans.Except
import qualified Data.Text.Encoding as E

import Servant
import Hasql.Connection

import Smugglers.Data
import Smugglers.DB

authCheck :: Connection -> BasicAuthCheck User
authCheck conn =
    let check (BasicAuthData email password) = do
            res <- runExceptT $ verifyUser conn email password
            case res of
                --Left (ServantErr code reason body hdrs) -> do
                --    print body
                Left _ -> return Unauthorized
                Right isValidUser ->
                    if isValidUser
                        then return (Authorized (User (E.decodeUtf8 email) (E.decodeUtf8 password)))
                        else return Unauthorized
    in BasicAuthCheck check
