{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : SimpleFCM.Apns
-- Description : Handles all the apple related issues for sending notif.
module SimpleFCM.Apns (
  mkContentAvailable,
  defaultApnsHeaders,
  defaultApnsOption,
  defaultApnsPayloadMessageOption,
  ApnsPriority(..),
  ApnsHeaders(..),
  ApnsOption(..),
  ApnsPayloadOption(..),
  ApsOption(..),
  ApsContentAvailable(..)
) where
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as DA
import Data.String (String)
import Protolude hiding ((&))

----------- IOS/APNS
-- | Limited between 0 to 10.
newtype ApnsPriority = ApnsPriority Int deriving (Show, Eq, Generic)
instance ToJSON ApnsPriority where
  toJSON (ApnsPriority v) = toJSON @Text . show $ v

newtype ApnsHeaders = ApnsHeaders {
  apnsPriority :: Maybe ApnsPriority
} deriving (Show, Eq, Generic)
instance ToJSON ApnsHeaders where
  toJSON = DA.genericToJSON apnsJSONOption
  toEncoding = DA.genericToEncoding apnsJSONOption
apnsJSONOption = DA.defaultOptions
    { DA.fieldLabelModifier = replaceApnsHeader,
      DA.omitNothingFields = True
    }
defaultApnsHeaders :: ApnsHeaders
defaultApnsHeaders = ApnsHeaders {
  apnsPriority = Nothing
}
data ApnsOption = ApnsOption {
  headers :: Maybe ApnsHeaders,
  payload :: Maybe ApnsPayloadOption
} deriving (Show, Eq, Generic, ToJSON)
defaultApnsOption :: ApnsOption
defaultApnsOption = ApnsOption {
  headers = Nothing,
  payload = Nothing
}
defaultApnsPayloadMessageOption = defaultApnsOption{
  payload = Just . ApnsPayloadOption . Just . ApsOption . mkContentAvailable $ 1
}
newtype ApnsPayloadOption = ApnsPayloadOption {
  aps :: Maybe ApsOption
} deriving (Show, Eq, Generic, ToJSON)
newtype ApsOption = ApsOption {
  contentAvailable :: Maybe ApsContentAvailable
} deriving (Show, Eq, Generic)
apsJSONOption = DA.defaultOptions
    { DA.fieldLabelModifier = replaceApsHeader,
      DA.omitNothingFields = True
    }
instance ToJSON ApsOption where
  toJSON = DA.genericToJSON apsJSONOption
  toEncoding = DA.genericToEncoding apsJSONOption
-- | Set to 1 for valid see : 
-- https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification
newtype ApsContentAvailable = ApsContentAvailable Int
 deriving (Show, Eq, Generic, ToJSON)

mkContentAvailable :: Int -> Maybe ApsContentAvailable
mkContentAvailable val 
  | val == 0 || val == 1 = Just $ ApsContentAvailable val
  | otherwise = Nothing

replaceApnsHeader :: String -> String
replaceApnsHeader "apnsPriority" = "apns-priority"
replaceApnsHeader a = a  
replaceApsHeader :: String -> String
replaceApsHeader "contentAvailable" = "content-available"
replaceApsHeader a = a  
