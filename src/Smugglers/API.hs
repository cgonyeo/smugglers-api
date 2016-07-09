{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.API where

import Servant
import Network.Wai
import Hasql.Connection

import Smugglers.Auth
import Smugglers.Data
import Smugglers.Handlers

app :: Connection -> Application
app conn = serveWithContext smugglersAPI (smugglersAPIContext conn) (server conn)

smugglersAPIContext :: (Connection -> Context (BasicAuthCheck User ': '[]))
smugglersAPIContext conn = (authCheck conn) :. EmptyContext

smugglersAPI :: Proxy SmugglersAPI
smugglersAPI = Proxy

type SmugglersAPI = "rums" :> BasicAuth "smugglers" User :> Get '[JSON] [Rum]
               :<|> "import-user" :> ReqBody '[JSON] User :> Post '[JSON] ()
               :<|> "update-rums" :> BasicAuth "smugglers" User :> Post '[JSON] ()

server :: Connection -> Server SmugglersAPI
server conn = getRumsForUser conn
         :<|> importUser conn
         :<|> updateRumsForUser conn
