{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Upstream.Auth (auth) where

import Data.Aeson
import qualified Network.HTTP.Simple as H
import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Servant
import qualified Data.Text.Encoding as E
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.IO.Class

import Smugglers.Data

data LoginInfo = LoginInfo { email            :: T.Text
                           , password         :: T.Text
                           , programId        :: Int
                           , rewardsGroupName :: T.Text
                           , referer          :: T.Text
                           } deriving (Show,Generic)
instance ToJSON LoginInfo

data LoginResponse = LoginResponse { redirect :: Bool
                                   , url      :: T.Text
                                   } deriving (Show,Generic)
instance FromJSON LoginResponse

data ErrorResponse = ErrorResponse { errors       :: Bool
                                   , errorMessage :: T.Text
                                   } deriving (Show,Generic)
instance FromJSON ErrorResponse

auth :: BS.ByteString -> BS.ByteString -> ExceptT ServantErr IO [Cookie]
auth email password = do
    let loginInfo = encode $ LoginInfo { email            = E.decodeUtf8 email
                                       , password         = E.decodeUtf8 password
                                       , programId        = 1
                                       , rewardsGroupName = "rumbustion"
                                       , referer          = "/brg"
                                       }
    let req = H.setRequestHost scHost
            $ H.setRequestPath authEndpoint
            $ H.setRequestMethod "POST"
            $ H.setRequestBodyLBS loginInfo
            $ H.addRequestHeader "Content-Type" "application/json; charset=UTF-8"
            $ H.defaultRequest
    res <- liftIO $ H.httpLBS req

    let body = H.getResponseBody res
    let cookies = H.getResponseHeader "Set-Cookie" res

    case decode body of
        Nothing -> case decode body of
                       Just (ErrorResponse _ msg) ->
                           throwE err401 { errBody =
                                BSL.fromStrict $ E.encodeUtf8 msg }
                       Nothing ->
                           throwE err500 { errBody = "something went wrong" }
        Just (LoginResponse r u) -> do
            when (not r) $
                throwE err500 { errBody = "unexpected redirect value" }
            when (u /= (E.decodeUtf8 dataEndpoint)) $
                throwE err500 { errBody = "unexpected redirect" }
            when (length cookies == 0) $
                throwE err500 { errBody = "no cookies were set" }
            return cookies
