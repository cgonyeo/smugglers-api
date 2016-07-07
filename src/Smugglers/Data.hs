{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Data where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import GHC.Generics

type Cookie = BS.ByteString

scHost :: BS.ByteString
scHost = "www.bevager.com"

authEndpoint :: BS.ByteString
authEndpoint = "/brg/login"

dataEndpoint :: BS.ByteString
dataEndpoint = "/brg/home?rewardsGroupName=rumbustion"

data Rum = Rum { country   :: T.Text
               , immortal  :: Bool
               , name      :: T.Text
               , price     :: T.Text
               , signed    :: Bool
               , signer    :: Maybe T.Text
               , requested :: Maybe T.Text
               , notes     :: T.Text
} deriving (Show,Generic)

instance ToJSON Rum

data User = User { userEmail     :: T.Text
                 , userFirstName :: T.Text
                 , userLastName  :: T.Text
                 , userCookies   :: [Cookie]
                 }
