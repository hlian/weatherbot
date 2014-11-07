{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Main where

import           BasePrelude
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Network.Wai.Handler.Warp (run)

-- Naked imports.
import           Control.Lens
import           Data.Aeson.Lens
import           Network.Linklater
import           Network.Wreq hiding (params)

port :: Int
port = 8888

config :: Config
config = Config "trello.slack.com" (error "you need to insert a token here")

urlOfQuery :: Text -> Text
urlOfQuery q = "http://api.openweathermap.org/data/2.5/weather?q=" <> q

temperatureOfUrl :: Text -> IO Double
temperatureOfUrl url = do
  response <- get (T.unpack url)
  let maybeTemperature = response ^? (responseBody . key "main" . key "temp" . _Double)
  case maybeTemperature of
   Just temperature -> return temperature
   Nothing -> error "No temperature found"

-- C-u C-c C-t to get this type to show up automatically.
present :: Double -> Text
present = T.pack . show

weatherBot :: Maybe Command -> IO Text
weatherBot (Just (Command user channel (Just query))) = do
  putStrLn ("+ Just got a query " <> show query)

  let url = urlOfQuery query
  temperature <- temperatureOfUrl url

  -- As Ian mentioned, it's weird we're not using the response. We
  -- should be, but we're not. To hide the warning, we're going to use
  -- `void :: IO a -> IO ()` to turn this `IO Response` into an `IO
  -- ()`.
  void $ say (message (present temperature)) config

  -- The HTTP response body for this command. If you have nothing nice
  -- to say, don't say anything at all. --Hao's grandma
  return ""

  where
    message :: Text -> Message
    message t = FormattedMessage (EmojiIcon "ghost") "weatherbot" channel (formats t)

    formats :: Text -> [Format]
    formats t = [FormatAt user, FormatString "It's currently", FormatString t, FormatString $ "Kelvin at " <> query <> "."]

weatherBot (Just (Command _ _ Nothing)) =
  return "Type `/weather [place]` for the weather at place!"

weatherBot _ =
  return "Sorry, wrong command?"

main :: IO ()
main = do
  putStrLn ("+ Listening on port " <> show port)
  run port (slashSimple weatherBot)
