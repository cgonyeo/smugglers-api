{-# LANGUAGE OverloadedStrings #-}
module Smugglers.DB where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Either
import Data.Functor.Contravariant (contramap)
import Data.Maybe
import Data.Semigroup ((<>))
import Contravariant.Extras.Contrazip

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Hasql.Connection
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Hasql.Query
import Hasql.Session
import Servant

import Smugglers.Data

wrapDBRun :: Connection -> BS.ByteString -> HE.Params a -> HD.Result b -> a -> ExceptT ServantErr IO b
wrapDBRun conn sqlBlob enc dec v = do
    let q = statement sqlBlob enc dec True
    res <- liftIO $ run (query v q) conn
    case res of
        Left e -> throwE err500 { errBody = BSL.pack $ show e }
        Right res -> return res

createUsersTableSQL :: BS.ByteString
createUsersTableSQL = "CREATE TABLE IF NOT EXISTS member ("
          `BS.append` "    id     SERIAL PRIMARY KEY,"
          `BS.append` "    email  TEXT   NOT NULL,"
          `BS.append` "    pwhash TEXT   NOT NULL,"
          `BS.append` "    UNIQUE (email)"
          `BS.append` ")"

createRumTableSQL :: BS.ByteString
createRumTableSQL = "CREATE TABLE IF NOT EXISTS rum ("
        `BS.append` "    id        SERIAL  PRIMARY KEY,"
        `BS.append` "    country   TEXT    NOT NULL,"
        `BS.append` "    name      TEXT    NOT NULL,"
        `BS.append` "    price     TEXT    NOT NULL,"
        `BS.append` "    immortal  BOOLEAN NOT NULL,"
        `BS.append` "    UNIQUE (name)"
        `BS.append` ")"

createDataTableSQL :: BS.ByteString
createDataTableSQL = "CREATE TABLE IF NOT EXISTS data ("
          `BS.append` "    id        SERIAL PRIMARY KEY,"
          `BS.append` "    user_id   INT    NOT NULL,"
          `BS.append` "    rum_id    INT    NOT NULL,"
          `BS.append` "    signer    TEXT,"
          `BS.append` "    requested TEXT,"
          `BS.append` "    notes     TEXT NOT NULL,"
          `BS.append` "    UNIQUE(user_id,rum_id)"
          `BS.append` ")"

initializeDB :: Settings -> ExceptT BS.ByteString IO Connection
initializeDB s = do econn <- liftIO $ acquire s
                    when (isLeft econn)
                        (throwE $ fromJust $ (\(Left x) -> x) econn)
                    let conn = (\(Right x) -> x) econn
                    let s = do sql createUsersTableSQL
                               sql createRumTableSQL
                               sql createDataTableSQL
                    res <- liftIO $ run s conn
                    case res of
                        Left e -> throwE $ BS.pack $ show e
                        Right _ -> return conn

insertUserSQL :: BS.ByteString
insertUserSQL = "INSERT INTO member (email,pwhash) VALUES ($1,crypt($2, gen_salt('bf')))"

insertUser :: Connection -> BS.ByteString -> BS.ByteString -> ExceptT ServantErr IO ()
insertUser conn email password = do
        let e = contrazip2 (HE.value HE.text) (HE.value HE.text)
            d = HD.unit
            v = (E.decodeUtf8 email,E.decodeUtf8 password)
        wrapDBRun conn insertUserSQL e d v

verifyUserSQL :: BS.ByteString
verifyUserSQL = "SELECT pwhash = crypt($2, pwhash) AND email = $1 FROM member;"

verifyUser :: Connection -> BS.ByteString -> BS.ByteString -> ExceptT ServantErr IO Bool
verifyUser conn email password = do
        let e = contrazip2 (HE.value HE.text) (HE.value HE.text)
            d = HD.singleRow $ HD.value HD.bool
            v = (E.decodeUtf8 email,E.decodeUtf8 password)
        wrapDBRun conn verifyUserSQL e d v

lookupUserIDSQL :: BS.ByteString
lookupUserIDSQL = "SELECT * FROM member WHERE email = $1"

lookupUserID :: Connection -> BS.ByteString -> ExceptT ServantErr IO Int
lookupUserID conn email = do
        let e = HE.value HE.text
            d = HD.singleRow $ HD.value HD.int4
            v = (E.decodeUtf8 email)
        fromIntegral <$> wrapDBRun conn lookupUserIDSQL e d v

lookupRumIDSQL :: BS.ByteString
lookupRumIDSQL = "SELECT * FROM rum WHERE name = $1"

lookupRumID :: Connection -> BS.ByteString -> ExceptT ServantErr IO Int
lookupRumID conn name = do
        let e = HE.value HE.text
            d = HD.singleRow $ HD.value HD.int4
            v = (E.decodeUtf8 name)
        fromIntegral <$> wrapDBRun conn lookupRumIDSQL e d v

getUserRumsSQL :: BS.ByteString
getUserRumsSQL = " SELECT rum.country"
     `BS.append` "      , rum.name"
     `BS.append` "      , rum.price"
     `BS.append` "      , rum.immortal"
     `BS.append` "      , d.signer"
     `BS.append` "      , d.requested"
     `BS.append` "      , d.notes"
     `BS.append` " FROM rum"
     `BS.append` " LEFT OUTER JOIN"
     `BS.append` "     ( SELECT *"
     `BS.append` "       FROM data"
     `BS.append` "       WHERE user_id = $1"
     `BS.append` "     ) AS d"
     `BS.append` " ON rum.id = d.rum_id;"

getUserRums :: Connection -> BS.ByteString -> ExceptT ServantErr IO [Rum]
getUserRums conn email = do
        userID <- lookupUserID conn email
        let e = HE.value HE.int4
            d = HD.rowsList $ (,,,,,,)
                                  <$> (HD.value HD.text)
                                  <*> (HD.value HD.text)
                                  <*> (HD.value HD.text)
                                  <*> (HD.value HD.bool)
                                  <*> (HD.nullableValue HD.text)
                                  <*> (HD.nullableValue HD.text)
                                  <*> (HD.nullableValue HD.text)
            v = (fromIntegral userID)
        rows <- wrapDBRun conn getUserRumsSQL e d v
        return $ map (\(c,na,p,i,s,r,no) -> Rum c na p i s r (toDefault no)) rows
    where toDefault (Just x) = x
          toDefault Nothing  = ""

upsertRumSQL :: BS.ByteString
upsertRumSQL = " INSERT INTO rum ( country"
   `BS.append` "                 , name"
   `BS.append` "                 , price"
   `BS.append` "                 , immortal"
   `BS.append` "                 )"
   `BS.append` " VALUES ($1,$2,$3,$4)"
   `BS.append` " ON CONFLICT (name)"
   `BS.append` " DO UPDATE SET ( country"
   `BS.append` "               , name"
   `BS.append` "               , price"
   `BS.append` "               , immortal"
   `BS.append` "               )"
   `BS.append` "     = ($1,$2,$3,$4)"

upsertRum :: Connection -> Rum -> ExceptT ServantErr IO ()
upsertRum conn (Rum c n p i _ _ _) = do
    let e = contrazip4 (HE.value HE.text)
                       (HE.value HE.text)
                       (HE.value HE.text)
                       (HE.value HE.bool)
        d = HD.unit
        v = (c,n,p,i)
    wrapDBRun conn upsertRumSQL e d v

upsertRumDataSQL :: BS.ByteString
upsertRumDataSQL = " INSERT INTO data ( user_id"
       `BS.append` "                  , rum_id"
       `BS.append` "                  , signer"
       `BS.append` "                  , requested"
       `BS.append` "                  , notes"
       `BS.append` "                  )"
       `BS.append` " VALUES ($1,$2,$3,$4,$5)"
       `BS.append` " ON CONFLICT (user_id,rum_id)"
       `BS.append` " DO UPDATE SET ( user_id"
       `BS.append` "               , rum_id"
       `BS.append` "               , signer"
       `BS.append` "               , requested"
       `BS.append` "               , notes"
       `BS.append` "               )"
       `BS.append` "     = ($1,$2,$3,$4,$5)"

upsertRumData :: Connection -> Int -> Rum -> ExceptT ServantErr IO ()
upsertRumData conn userID (Rum _ n _ _ s r no) = do
    rumID <- lookupRumID conn (E.encodeUtf8 n)
    let e = contrazip5 (HE.value HE.int4)
                       (HE.value HE.int4)
                       (HE.nullableValue HE.text)
                       (HE.nullableValue HE.text)
                       (HE.value HE.text)
        d = HD.unit
        v = (fromIntegral userID,fromIntegral rumID,s,r,no)
    wrapDBRun conn upsertRumDataSQL e d v

saveRumsForUser :: Connection -> BS.ByteString -> [Rum] -> ExceptT ServantErr IO ()
saveRumsForUser conn email rums = do
    forM_ rums (upsertRum conn)
    let rumsWithData = filter (\(Rum _ _ _ _ r q n) -> not (isNothing r && isNothing q && n == "")) rums
    userID <- lookupUserID conn email
    forM_ rumsWithData (upsertRumData conn userID)
    return ()
