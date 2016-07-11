{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (run)

import Options.Applicative
import Smugglers.API
import Smugglers.DB
import Control.Monad.Trans.Except
import GHC.Word (Word16)

import qualified Hasql.Connection as H
import qualified Data.ByteString.Char8 as BS

data Settings = Settings String Word16 String String String
convSettings :: Settings -> H.Settings
convSettings (Settings h p u pa d) = H.settings (BS.pack h)
                                                p
                                                (BS.pack u)
                                                (BS.pack pa)
                                                (BS.pack d)

confParser :: Parser Settings
confParser = Settings <$> strOption
                             ( long "host"
                            <> help "the postgres host to connect to"
                             )
                        <*> option auto
                             ( long "port"
                            <> help "the postgres port to connect on"
                             )
                        <*> strOption
                             ( long "user"
                            <> help "the postgres user to auth as"
                             )
                        <*> strOption
                             ( long "password"
                            <> help "the postgres password to auth with"
                             )
                        <*> strOption
                             ( long "database"
                            <> help "the postgres database to use"
                             )

main :: IO ()
main = do
        econn <- execParser opts >>= (runExceptT . initializeDB . convSettings)
        case econn of
            Left err -> BS.putStrLn err
            Right conn -> run 8080 (app conn)
    where
        opts = info (helper <*> confParser)
          ( fullDesc
         <> header "smugglers-api - a wrapper for sc.bevager.com"
          )
