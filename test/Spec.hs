{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude
import SimpleFCM.FCMV1 (sendMessageWithAccessToken, sendMessage, simpleMessage)
import SimpleFCM.TokenContainer (generateBothToken, tokenUpdaterThread, GoogleTokenContainer(..), GoogleAccessToken, GoogleMainToken, TokenSettings (TokenSettings), getFCMAccessToken, getToken)
import Data.String (String)

data TestTokenContainer = TestTokenContainer {
      mainToken :: GoogleMainToken,
      accessToken :: GoogleAccessToken
} deriving (Eq, Show)

instance GoogleTokenContainer TestTokenContainer where
  setGoogleMainToken a b = b{mainToken = a}
  getGoogleMainToken = mainToken
  setGoogleAccessToken a b = b{accessToken = a}
  getGoogleAccessToken = accessToken

privateKey :: String
privateKey = ""

defaultSettings :: TokenSettings
defaultSettings = TokenSettings "" privateKey ""

main :: IO ()
main = do
  print =<< (runExceptT $ do
    (_, access_token) <- generateBothToken defaultSettings
    liftIO $ sendMessageWithAccessToken (access_token) defaultSettings (simpleMessage "allUsers" "ok" "ok"))

  -- tokenCont <- case (uncurry TestTokenContainer) <$> res of
  --   Left err -> panic err
  --   Right r -> newMVar r
  -- tokenUpdaterThread (Just $ 1000000 * 3) defaultSettings tokenCont
  -- printPeriodicValue tokenCont
  -- pure ()
  -- where
  --   printPeriodicValue tokenCont = do
  --     res <- readMVar tokenCont
  --     putLText $ "Read mvar = " <> show res
  --     threadDelay (1000000 * 1)
  --     printPeriodicValue tokenCont
