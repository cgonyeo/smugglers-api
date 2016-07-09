{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Auth where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E

import Servant

import Smugglers.Data
import Smugglers.DB
import Smugglers.Upstream.Auth

import Hasql.Connection

authCheck :: Connection -> BasicAuthCheck User
authCheck conn =
    let check (BasicAuthData email password) = do
            res <- runExceptT $ verifyUser conn email password
            case res of
                Left (ServantErr code reason body hdrs) -> do
                    print body
                    return Unauthorized
                Right isValidUser ->
                    if isValidUser
                        then return (Authorized (User (E.decodeUtf8 email) (E.decodeUtf8 password)))
                        else return Unauthorized
    in BasicAuthCheck check
    --let check (BasicAuthData email password) = do
    --        res <- runExceptT $ do
    --            cookies <- auth email password
    --            insertUser conn email password
    --            return cookies
    --        case res of
    --            Left (ServantErr code reason body hdrs) -> do
    --                print body
    --                return Unauthorized
    --            Right cookies ->
    --                return (Authorized (User (E.decodeUtf8 email) "drek" "gnyo" cookies))
    --in BasicAuthCheck check
