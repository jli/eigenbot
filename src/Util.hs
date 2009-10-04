module Util (
    (+++)
  , appFst
  , calTimeString
  , getCalTime
  , breakOnSpace
  , delay
  , eitherToMaybe
  , maybeIO
  , getUrl
  , headUrl
  , probablyUrl
  , simpleHttp
) where

import Control.Concurrent (threadDelay)
import Data.List (isPrefixOf)
import Network.Browser (browse, setAllowRedirects, setMaxRedirects, request)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Network.HTTP.Base (Response(..), Request(..), RequestMethod(..))
import Network.HTTP.Headers (Header(..))
import System.Time (CalendarTime(..), getClockTime, toCalendarTime)
import Text.Printf(printf)

-- needs shorter name
(+++) :: String -> String -> String
"" +++ "" = ""
s1 +++ "" = s1
"" +++ s2 = s2
s1 +++ s2 = s1 ++ (' ':s2)

calTimeString :: CalendarTime -> String
calTimeString ct =
    printf "%04d-%02d-%02d" (ctYear ct) (fromEnum $ ctMonth ct) (ctDay ct)

getCalTime :: IO CalendarTime
getCalTime = getClockTime >>= toCalendarTime

appFst :: (a -> z) -> (a, b) -> (z, b)
appFst f (one, two) = (f one, two)

breakOnSpace :: String -> (String, String)
breakOnSpace = break (== ' ')

delay :: Int -> IO ()
delay = threadDelay . (* 10^(6::Integer)) -- better way to do this typehint?

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeIO :: (a -> IO ()) -> Maybe a -> IO ()
maybeIO f m = maybe (return ()) f m

-- must be a better way, but example in Network.Browser docs no longer worked
fetchUrl :: String -> RequestMethod -> IO (Response String)
fetchUrl url method = do
      (_, rsp) <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               setMaxRedirects $ Just 5
               request $ ((getRequest url) { rqMethod = method })
      return rsp

getUrl :: String -> IO String
getUrl url = fetchUrl url GET >>= getResponseBody . Right

headUrl :: String -> IO [Header]
headUrl url = fetchUrl url HEAD >>= return . rspHeaders

probablyUrl :: String -> Bool
probablyUrl s =
    "http://" `isPrefixOf` s ||
    "https://" `isPrefixOf` s

simpleHttp :: String -> IO String
simpleHttp url = simpleHTTP (getRequest url) >>= getResponseBody
