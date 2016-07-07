{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Auth where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as E

import Servant

import Smugglers.Data
import Smugglers.Upstream.Auth

authCheck :: BasicAuthCheck User
authCheck =
    let check (BasicAuthData email password) = do
            res <- runExceptT $ auth email password
            case res of
                Left (ServantErr code reason body hdrs) ->
                    return Unauthorized
                Right cookies ->
                    return (Authorized (User (E.decodeUtf8 email) "drek" "gnyo" cookies))
    in BasicAuthCheck check
