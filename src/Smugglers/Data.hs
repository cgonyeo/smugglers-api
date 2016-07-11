{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Smugglers.Data where

import Text.Read (readMaybe)
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

noteEndpoint :: BS.ByteString
noteEndpoint = "/brg/save-notes"

data Rum = Rum { rumId      :: Int
               , country    :: T.Text
               , name       :: T.Text
               , price      :: Double
               , immortal   :: Bool
               , signer     :: Maybe T.Text
               , requested  :: Maybe T.Text
               , notes      :: Maybe StructuredNote
               } deriving (Show,Generic)

instance ToJSON Rum
instance FromJSON Rum

data User = User { userEmail    :: T.Text
                 , userPassword :: T.Text
                 } deriving (Generic)
instance ToJSON User
instance FromJSON User

data StructuredNote = StructuredNote { rating     :: Double
                                     , color      :: T.Text
                                     , smells     :: T.Text
                                     , taste      :: T.Text
                                     , aftertaste :: T.Text
                                     , thoughts   :: T.Text
                                     } deriving(Generic)
instance ToJSON StructuredNote
instance FromJSON StructuredNote

instance Show StructuredNote where
        show (StructuredNote 0.0 "" "" "" "" th) = T.unpack th
        show (StructuredNote r c s ta a th) =
            "--Rating--\n"          ++ show r
         ++ "\n\n--Color--\n"       ++ T.unpack c
         ++ "\n\n--Smells--\n"      ++ T.unpack s
         ++ "\n\n--Taste--\n"       ++ T.unpack ta
         ++ "\n\n--After Taste--\n" ++ T.unpack a
         ++ "\n\n--Thoughts--\n"    ++ T.unpack th

parseStructuredNote :: String -> Maybe StructuredNote
parseStructuredNote "" = Nothing
parseStructuredNote str = 
        let ls = lines str
            sections = foldl f [] ls
            maybeNote =
                do r  <-            lookup "--Rating--"      sections >>= readMaybe
                   c  <- T.pack <$> lookup "--Color--"       sections
                   s  <- T.pack <$> lookup "--Smells--"      sections
                   ta <- T.pack <$> lookup "--Taste--"       sections
                   a  <- T.pack <$> lookup "--After Taste--" sections
                   th <- T.pack <$> lookup "--Thoughts--"    sections
                   return $ StructuredNote r c s ta a th
        in case maybeNote of
               Just n -> Just n
               Nothing -> Just $ StructuredNote 0.0 "" "" "" "" (T.pack str)
    where f a l@('-':'-':_) = (l,""):a
          f ((label,""):a) l = (label,l):a
          f ((label,contents):a) l = (label,contents ++ "\n" ++ l):a
          f a _ = a
