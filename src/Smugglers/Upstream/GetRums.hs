{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Upstream.GetRums (getRumsForUser) where

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
import Text.HTML.TagSoup
import qualified Network.HTTP.Simple as H

import Smugglers.Data

getRumsForUser :: [Cookie] -> ExceptT ServantErr IO [Rum]
getRumsForUser authCookies = do
    html <- getRumHTML authCookies
    let tags = parseRumHTML html
        mrums = map lineToRum tags
        rums = catMaybes mrums
    return rums

getRumHTML :: [Cookie] -> ExceptT ServantErr IO BS.ByteString
getRumHTML cookies = do
    let req = H.setRequestHost scHost
            $ H.setRequestPath dataEndpoint
            $ H.setRequestMethod "GET"
            $ H.setRequestHeader "Cookie" cookies
            $ H.defaultRequest
    res <- liftIO $ H.httpLBS req
    return $ BSL.toStrict $ H.getResponseBody res

parseRumHTML :: BS.ByteString -> [[Tag BS.ByteString]]
parseRumHTML html = let tags = parseTags html
                        tagGroups = foldl (\a x -> f a x) [[]] tags
                    in reverse tagGroups
        where f (curr:cs) t@(TagOpen "tr" atrs) = if hasClass "item" atrs then ((t:curr):cs) else (curr:cs)
              f (curr:cs) t@(TagClose "tr") = ([]:(reverse $ t:curr):cs)
              f ([]:cs) _ = ([]:cs)
              f (curr:cs) t = ((t:curr):cs)

hasClass :: BS.ByteString -> [Attribute BS.ByteString] -> Bool
hasClass c attrs = case getAttr "class" attrs of
                       Just val -> c `elem` BS.split ' ' val
                       Nothing  -> False

getAttr :: BS.ByteString -> [Attribute BS.ByteString] -> Maybe BS.ByteString
getAttr _ [] = Nothing
getAttr c ((k,v):as)
    | k == c    = Just v
    | otherwise = getAttr c as

lineToRum :: [Tag BS.ByteString] -> Maybe Rum
lineToRum [ TagOpen "tr" dateReqAttrs -- Rum without notes
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagText country
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "a" linkAttrs
          , TagText name
          , TagClose "a"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText cost
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "span" _
          , TagClose "span"
          , TagText signer
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagClose "tr"
          ] = Just $ newRum country linkAttrs name cost signer dateReqAttrs ""
lineToRum [ TagOpen "tr" dateReqAttrs -- Rum with notes
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagText country
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "a" linkAttrs
          , TagText name
          , TagClose "a"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText cost
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "span" _
          , TagClose "span"
          , TagText signer
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagText notes
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagClose "tr"
          ] = Just $ newRum country linkAttrs name cost signer dateReqAttrs notes
lineToRum [ TagOpen "tr" _ -- Rum they haven't had with notes
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagText country
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" []
          , TagText _
          , TagOpen "a" linkAttrs
          , TagText name
          , TagClose "a"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" []
          , TagText cost
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagText notes
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagClose "tr"
          ] = Just $ newRum country linkAttrs name cost "" [] notes
lineToRum [ TagOpen "tr" _ -- Rum they haven't had with no notes
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagText country
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" []
          , TagText _
          , TagOpen "a" linkAttrs
          , TagText name
          , TagClose "a"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" []
          , TagText cost
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagClose "td"
          , TagText _
          , TagOpen "td" _
          , TagText _
          , TagOpen "div" _
          , TagClose "div"
          , TagText _
          , TagClose "td"
          , TagText _
          , TagClose "tr"
          ] = Just $ newRum country linkAttrs name cost "" [] ""
lineToRum _ = Nothing

newRum :: BS.ByteString
       -> [Attribute BS.ByteString]
       -> BS.ByteString
       -> BS.ByteString
       -> BS.ByteString
       -> [Attribute BS.ByteString]
       -> BS.ByteString
       -> Rum
newRum country linkAttrs name cost  signer dateReqAttrs notes
            = Rum (parseUpstreamID linkAttrs)
                  (E.decodeUtf8 $ stripWhitespace country)
                  (E.decodeUtf8 $ stripWhitespace name)
                  (E.decodeUtf8 $ stripWhitespace cost)
                  (hasClass "immortal-item" linkAttrs)
                  (E.decodeUtf8 <$> parseSigner (stripWhitespace signer))
                  (E.decodeUtf8 <$> getAttr "data-requested" dateReqAttrs)
                  (parseStructuredNote $ BS.unpack $ stripWhitespace notes)

parseSigner :: BS.ByteString -> Maybe BS.ByteString
parseSigner "" = Nothing
parseSigner s  = Just s

parseUpstreamID :: [Attribute BS.ByteString] -> Int
parseUpstreamID attrs =
    let ngClick = getAttr "ng-click" attrs
        f :: Maybe BS.ByteString -> BS.ByteString
        f (Just str) = BS.takeWhile (/= ',') $ BS.tail $ BS.dropWhile (/= ',') str
        f Nothing = error "ng-click attribute was unexpected!"
        g :: BS.ByteString -> Int
        g str = read $ BS.unpack $ stripWhitespace str
    in g (f ngClick)

--stripWhitespace :: BS.ByteString -> BS.ByteString
--stripWhitespace input = BS.unwords $ filter (\x -> not $ x `elem` ["","\n","\t"]) $ BS.split ' ' input

stripWhitespace :: BS.ByteString -> BS.ByteString
stripWhitespace input = let strippedBeginning = dropWhiteSpaceAtFront input
                        in BS.reverse (dropWhiteSpaceAtFront $ BS.reverse strippedBeginning)
        where dropWhiteSpaceAtFront input =
                        let mi = BS.findIndex (\c -> not $ c `elem` [' ', '\n', '\t']) input
                        in case mi of
                               Just i -> BS.drop i input
                               Nothing -> ""
