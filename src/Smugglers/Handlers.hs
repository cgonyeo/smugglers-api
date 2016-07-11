module Smugglers.Handlers where

import Servant
import Control.Monad.Trans.Except
import qualified Data.Text.Encoding as E

import Smugglers.Data
import Smugglers.DB
import qualified Smugglers.Upstream.GetRums as R
import qualified Smugglers.Upstream.UpdateNote as U

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import Smugglers.Upstream.Auth

import Hasql.Connection

importUser :: Connection -> User -> ExceptT ServantErr IO ()
importUser conn (User email pw) = do
    let bsEmail = E.encodeUtf8 email
        bsPw = E.encodeUtf8 pw
    authCookies <- auth bsEmail bsPw
    insertUser conn bsEmail bsPw
    rums <- R.getRumsForUser authCookies
    saveRumsForUser conn bsEmail rums

updateRumsForUser :: Connection -> User -> ExceptT ServantErr IO ()
updateRumsForUser conn (User email pw) = do
    let bsEmail = E.encodeUtf8 email
    authCookies <- auth bsEmail (E.encodeUtf8 pw)
    rums <- R.getRumsForUser authCookies
    saveRumsForUser conn bsEmail rums

getRumsForUser :: Connection -> User -> ExceptT ServantErr IO [Rum]
getRumsForUser conn (User email _) = getUserRums conn (E.encodeUtf8 email)

updateNoteForUser :: Connection -> User -> Int -> StructuredNote -> ExceptT ServantErr IO ()
updateNoteForUser conn (User email pw) rumID newNote = do
    let bsEmail = E.encodeUtf8 email
    authCookies <- auth bsEmail (E.encodeUtf8 pw)
    U.updateNoteForUser authCookies rumID (BS.pack $ show newNote)
    upsertNoteForUser conn bsEmail rumID newNote
