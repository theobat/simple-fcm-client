{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SimpleFCM.FCMV1 where
import Protolude hiding ((&))
import Data.String (String, IsString(fromString))
import qualified Network.Wreq as NW
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import Control.Lens ( (&), (^.), (.~) )
import Network.HTTP.Types.Status (statusIsSuccessful)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Data.Text (pack)
import Data.HashMap.Strict (fromList)
import Data.Aeson (ToJSON(toJSON))
import SimpleFCM.TokenContainer (getGoogleAccessToken, GoogleTokenContainer)

newtype ProjectId = ProjectId Text
  deriving newtype (Eq, Show)

instance IsString ProjectId where
  fromString = ProjectId . pack  

data FCMNotification = FCMNotification
  { title :: Text
  , body :: Text
  } deriving (Show, Eq, Generic, ToJSON)

data FCMMessage = FCMMessage
  { notification :: FCMNotification,
    payload :: Maybe DA.Object,
    topic :: Maybe Text,
    token :: Maybe Text
  } deriving (Show, Eq, Generic)

simpleMessage :: Text -> Text -> Text -> FCMMessage
simpleMessage topicV titleV bodyV = FCMMessage {
    notification = FCMNotification {
      title = titleV,
      body = bodyV
    },
    payload = Nothing,
    topic = Just topicV,
    token = Nothing
  }

replaceData :: String -> String
replaceData "payload" = "data"
replaceData a = a

customOptions :: DA.Options
customOptions = DA.defaultOptions
  { DA.fieldLabelModifier = replaceData
  }

instance DA.ToJSON FCMMessage where
    toJSON     = DA.genericToJSON customOptions
    toEncoding = DA.genericToEncoding customOptions

sendMessage :: GoogleTokenContainer containerT => containerT -> ProjectId -> FCMMessage -> IO (Either NW.Status NW.Status)
sendMessage settings projectId fcmMsg = do
  let token = show $ getGoogleAccessToken settings
  let authHeader = "Bearer " <> token
  print authHeader
  let opts = NW.defaults & NW.header "Authorization" .~ [encodeUtf8 authHeader]
      msgVal=DA.toJSON $ Msg fcmMsg
  resp <- NW.postWith opts ("https://fcm.googleapis.com/v1/projects/" <> (show projectId) <> "/messages:send") msgVal
  putStrLn ("Resp:" <> show resp::Text)
  let respStatus = resp ^. NW.responseStatus
  pure $ (if statusIsSuccessful respStatus then Right else Left) respStatus

data Msg a = Msg { message :: a} deriving (Eq, Show, Generic, ToJSON)