-- improvements:
-- - filter out URLs to prevent abuse
-- - only allow port 80?

module Plugins.UrlTitle (plugin) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (mapM_)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust, isJust, listToMaybe)

import Network.HTTP.Headers (Header(..), HeaderName(..))
import Text.HTML.TagSoup (Tag(..), sections, (~==))
import Text.HTML.TagSoup.Parser (parseTags)

import qualified Base as B
import Util (io, mcoin, getUrl, headUrl, probablyUrl, maybeIO, eitherToMaybe)

plugin :: B.Plugin
plugin = B.genPlugin "urltitle: fetches titles" loop () Nothing

loop :: B.PluginLoop ()
loop evq actq = do
    ev <- io $ B.readEvent evq
    case ev of
      B.ChannelMsg chan _nick msg -> parseAndAnnounce actq chan $ words msg
      _ -> mcoin

parseAndAnnounce :: B.ActQ -> B.Channel -> [String] -> B.PluginStateT ()
parseAndAnnounce actq chan strs =
    io $ mapM_ (forkIO . announceMaybe) $ filter probablyUrl strs
  where announceMaybe url = do
          maybeTitle <- getCleanTitle url
          maybeIO sayTitle maybeTitle
        sayTitle = B.say actq chan . ("title: " ++ )

-- hmm, make this more expansive? \n\r case found here:
-- http://www.cityofgainesville.org/tabid/156/Default.aspx
-- tabs found on YouTube, wtf
cleanTitle :: String -> String
cleanTitle = map tabToSpace . filter (not . lineEnding)
  where lineEnding = (`elem` ['\n', '\r'])
        tabToSpace '\t' = ' '
        tabToSpace  c   = c

getCleanTitle :: String -> IO (Maybe String)
getCleanTitle url = getTitle url >>= return . (fmap cleanTitle)

-- checks that the URL is "safe" to get
getTitle :: String -> IO (Maybe String)
getTitle url = do
    okay <- okayToGet url
    case okay of
      False -> return Nothing
      True -> getTitle' url

getTitle' :: String -> IO (Maybe String)
getTitle' url = do
    e <- try $ getUrl url :: IO (Either SomeException String)
    return (eitherToMaybe e >>=
            listToMaybe . sections (~== "<title>") . parseTags >>=
            fromTitle)
  where fromTitle ((TagOpen "title" []) : (TagText t) : _) = Just t
        fromTitle _ = Nothing

okayToGet :: String -> IO Bool
okayToGet url = headUrl url >>= return . headersOkay

-- bugz:
-- http://www.eng.uwaterloo.ca/~aavogt/xmonad/docs/XMonad-Actions-TopicSpace.html
-- FIXME: wrap read for safety
-- Okay if either Content-Type starts with "text" or if Content-Length < 1mb
headersOkay :: [Header] -> Bool
headersOkay hs = lengthOkay || contentOkay
  where lengthOkay = isJust len && (read (fromJust len) :: Integer) < oneMb
        contentOkay = isJust content && "text" `isPrefixOf` fromJust content
        oneMb = 10^(6::Integer)
        len = getHdr HdrContentLength hs
        content = getHdr HdrContentType hs
        getHdr hdr headers = -- kind of bad
            case find (\(Header name _) -> name == hdr) headers of
              Nothing -> Nothing
              Just (Header _ val) -> Just val
