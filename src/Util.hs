module Util (
    (+++)
  , (<||>)
  , (<&&>)
  , appFst
  , concatComma
  , dropLast
  , calTimeString
  , getCalTime
  , plural
  , ellipsesSplit
  , generalPlural
  , breakOnSpace
  , dropNewlines
  , delay
  , doForever
  , mcoin
  , eitherToMaybe
  , io
  , maybeM
  , lookupExn
  , insertCons
  , insertAppend
  , getUrl
  , headUrl
  , probablyUrl
  , simpleHttp
  , safeIndex
) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (group, intersperse, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
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

(<&&>), (<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p1 <&&> p2 = \a -> p1 a && p2 a
p1 <||> p2 = \a -> p1 a || p2 a

calTimeString :: CalendarTime -> String
calTimeString ct =
    printf "%04d-%02d-%02d" (ctYear ct) month (ctDay ct)
  where month = fromEnum (ctMonth ct) + 1

getCalTime :: IO CalendarTime
getCalTime = getClockTime >>= toCalendarTime

generalPlural :: String -> String -> Integer -> String
generalPlural one _many 1 = "1" +++ one
generalPlural _one many n = show n +++ many

plural :: String -> Integer -> String
plural one n = generalPlural one (one++"s") n

appFst :: (a -> z) -> (a, b) -> (z, b)
appFst f (one, two) = (f one, two)

concatComma :: [String] -> String
concatComma = concat . intersperse ", "

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (max 0 ((length xs) - n)) xs

breakOnSpace :: String -> (String, String)
breakOnSpace = break (== ' ')

dropNewlines :: String -> String
dropNewlines = map nlToSpace
  where nlToSpace '\n' = ' '
        nlToSpace '\r' = ' '
        nlToSpace c = c

-- This function returns Nothing if the limit is too small. The
-- resulting strings in general may require eLen*2 amounts of padding,
-- at the front and back, so the limit must be strictly greater than
-- eLen*2.
ellipses :: String
ellipsesLen :: Int
ellipses = ".."
ellipsesLen = length ellipses

ellipsesSplit :: String -> Int -> Maybe [String]
ellipsesSplit s lim | lim <= ellipsesLen*2 = Nothing
                    | otherwise = Just $ safeSplit s lim

safeSplit :: String -> Int -> [String]
safeSplit s lim | length s <= lim = [s]
                | otherwise       = first' : safeSplit rest' lim
  where (first, rest) = splitAt (lim-ellipsesLen) s
        first' = first ++ ellipses
        rest' = ellipses ++ rest

delay :: Int -> IO ()
delay = threadDelay . (* 10^(6::Integer)) -- better way to do this typehint?

doForever :: IO a -> IO ThreadId
doForever = forkIO . forever

mcoin :: Monad m => m ()
mcoin = return ()

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

io :: MonadIO m => IO a -> m a
io = liftIO

maybeM :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM = maybe mcoin

lookupExn :: Ord k => k -> Map k v -> v
lookupExn k v = fromJust $ M.lookup k v

insertCons :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertCons k v = M.insertWith' (++) k [v]

insertAppend :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertAppend k v = M.insertWith' (flip (++)) k [v]

-- must be a better way, but example in Network.Browser docs no longer worked
fetchUrl :: String -> RequestMethod -> IO (Response String)
fetchUrl url method = do
      (_, rsp) <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               setMaxRedirects $ Just 5
               request ((getRequest url) { rqMethod = method })
      return rsp

getUrl :: String -> IO String
getUrl url = fetchUrl url GET >>= getResponseBody . Right

headUrl :: String -> IO [Header]
headUrl url = rspHeaders `fmap` fetchUrl url HEAD

probablyUrl :: String -> Bool
probablyUrl s =
    "http://" `isPrefixOf` s ||
    "https://" `isPrefixOf` s

simpleHttp :: String -> IO String
simpleHttp url = simpleHTTP (getRequest url) >>= getResponseBody

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex xs i = Just $ xs !! (abs i `mod` length xs)
