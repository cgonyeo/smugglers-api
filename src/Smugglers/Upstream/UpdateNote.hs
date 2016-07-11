{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Upstream.UpdateNote (updateNoteForUser) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Maybe
import GHC.Generics

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Servant
import qualified Network.HTTP.Simple as H

import qualified Smugglers.Data as D

data NewNoteInfo = NewNoteInfo { itemId           :: Int
                               , notes            :: T.Text
                               , rewardsGroupName :: T.Text
                               } deriving(Generic)
instance ToJSON NewNoteInfo

updateNoteForUser :: [D.Cookie] -> Int -> BS.ByteString -> ExceptT ServantErr IO ()
updateNoteForUser authCookies rumID newNote = do
    let newNoteInfo = encode $ NewNoteInfo { itemId = rumID
                                           , notes  = E.decodeUtf8 newNote
                                           , rewardsGroupName = "rumbustion"
                                           }
    let req = H.setRequestHost D.scHost
            $ H.setRequestPath D.noteEndpoint
            $ H.setRequestMethod "POST"
            $ H.setRequestBodyLBS newNoteInfo
            $ H.addRequestHeader "Content-Type" "application/json; charset=UTF-8"
            $ H.setRequestHeader "Cookie" authCookies
            $ H.defaultRequest
    res <- liftIO $ H.httpLBS req
    let code = H.getResponseStatusCode res
    when (code /= 200) $ throwE err500 { errBody =
            BSL.pack $ "unexpected status code: " ++ show code }
    let body = H.getResponseBody res
    when (body /= "") $ throwE err500 { errBody =
            "unexpected body: " `BSL.append` body }
