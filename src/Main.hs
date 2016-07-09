{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp

import Smugglers.API
import Smugglers.DB
import Control.Monad.Trans.Except

import qualified Hasql.Connection as H
import qualified Data.ByteString.Char8 as BS

settings :: H.Settings
settings = H.settings "localhost" 5432 "smugglers" "smugglers" "smugglers"

main :: IO ()
main = do
    econn <- runExceptT $ initializeDB settings
    case econn of
        Left err -> BS.putStrLn err
        Right conn -> run 8080 (app conn)
