{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Smugglers.Client where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client

import Network.HTTP.Client

import Smugglers.Data
import Smugglers.API

getRums    :: BasicAuthData -> Manager -> BaseUrl -> ExceptT ServantError IO [Rum]
importUser :: User -> Manager -> BaseUrl -> ExceptT ServantError IO ()
updateRums :: BasicAuthData -> Manager -> BaseUrl -> ExceptT ServantError IO ()
updateNote :: BasicAuthData -> Int -> StructuredNote -> Manager -> BaseUrl -> ExceptT ServantError IO ()

getRums
 :<|> importUser
 :<|> updateRums
 :<|> updateNote
    = client smugglersAPI
