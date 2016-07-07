{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.API where

import Servant
import Network.Wai

import Smugglers.Auth
import Smugglers.Data
import Smugglers.Handlers.RumList

app :: Application
app = serveWithContext smugglersAPI smugglersAPIContext server

smugglersAPIContext :: Context (BasicAuthCheck User ': '[])
smugglersAPIContext = authCheck :. EmptyContext

smugglersAPI :: Proxy SmugglersAPI
smugglersAPI = Proxy

type SmugglersAPI = "rums" :> BasicAuth "smugglers" User :> Get '[JSON] [Rum]

server :: Server SmugglersAPI
server = getRumsForUser
