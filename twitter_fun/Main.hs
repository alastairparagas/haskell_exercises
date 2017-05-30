module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-), ($=+), runConduit)
import Data.Conduit.List (mapM_, map, filter, catMaybes)
import qualified Data.ByteString.Char8 (pack)
import Data.Text (unpack)
import qualified Data.Text (pack)
import Data.Maybe (fromJust)
import Configuration.Dotenv (loadFile, onMissingFile)
import System.Environment (lookupEnv)
import System.Exit (die)
import Web.Authenticate.OAuth (oauthConsumerKey, oauthConsumerSecret)
import Web.Twitter.Conduit (Credential(Credential), newManager, tlsManagerSettings)
import Web.Twitter.Types 
  (StreamingAPI(SStatus, SRetweetedStatus)
  , Status(Status), statusText, statusLang 
  , RetweetedStatus(RetweetedStatus), rsRetweetedStatus
  )
import Web.Twitter.Conduit.Stream (statusesFilterByTrack, stream)
import Web.Twitter.Conduit.Types 
  (TWInfo(TWInfo), TWToken(TWToken)
    , twitterOAuth, twToken, twOAuth, twCredential, twProxy
  )


filterEnglishTweets :: StreamingAPI -> Bool
filterEnglishTweets tweet = 
  let
    langIsEnglish (Status {statusLang=language}) = case language of 
      Just "en" -> True
      _ -> False
  in case tweet of 
    SStatus statusObj -> langIsEnglish statusObj
    SRetweetedStatus (RetweetedStatus {rsRetweetedStatus=statusObj}) -> 
      langIsEnglish statusObj
    _ -> False

  
tweetParser :: StreamingAPI -> Maybe String
tweetParser tweet = case tweet of 
  SStatus (Status {statusText=status}) -> Just $ unpack status
  SRetweetedStatus (RetweetedStatus {rsRetweetedStatus=rstatus}) -> 
    Just $ unpack $ statusText rstatus
  _ -> Nothing


main :: IO ()
main = do
  
  let 
    loadFileIO = 
      loadFile False "./.env"
    errorHandlerIO = 
      putStrLn ".env file is missing! Using global env instead!"
    in onMissingFile loadFileIO errorHandlerIO
    
  consumerKey <- lookupEnv "TWITTER_CONSUMER_KEY" 
  consumerSecret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  accessToken <- lookupEnv "TWITTER_ACCESS_TOKEN"
  accessTokenSecret <- lookupEnv "TWITTER_ACCESS_TOKEN_SECRET"
  case (consumerKey, consumerSecret, accessToken, accessTokenSecret) of 
    (Just _, Just _, Just _, Just _) -> return ()
    (_, _, _, _) -> die "required Environment Variables not set!"
  
  connectionManager <- newManager tlsManagerSettings
  
  let 
    apiRequest = statusesFilterByTrack $ Data.Text.pack "*"
    twitterInfo = TWInfo {
      twToken = TWToken {
        twOAuth = twitterOAuth {
          oauthConsumerKey = (Data.ByteString.Char8.pack . fromJust) consumerKey, 
          oauthConsumerSecret = (Data.ByteString.Char8.pack . fromJust) consumerSecret
        },
        twCredential = Credential [
          (
            Data.ByteString.Char8.pack "oauth_token", 
            (Data.ByteString.Char8.pack . fromJust) accessToken
          ), 
          (
            Data.ByteString.Char8.pack "oauth_token_secret", 
            (Data.ByteString.Char8.pack . fromJust) accessTokenSecret
          )
        ]
      }, 
      twProxy = Nothing
    }
    
    in runResourceT $ do 
      stream <- stream twitterInfo connectionManager apiRequest
      stream $=+ 
        Data.Conduit.List.filter filterEnglishTweets $=+
        Data.Conduit.List.map tweetParser $=+ 
        Data.Conduit.List.catMaybes $$+- 
        Data.Conduit.List.mapM_ (liftIO . putStrLn)
