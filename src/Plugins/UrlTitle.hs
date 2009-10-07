-- improvements:
-- - filter out URLs to prevent abuse
-- - only allow port 80?

module Plugins.UrlTitle (plugin) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (mapM_)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)

import Network.HTTP.Headers (Header(..), HeaderName(..))
import Text.HTML.TagSoup (Tag(..), sections, (~==))
import Text.HTML.TagSoup.Parser (parseTags)

import qualified Base as B
import Util (io, mcoin, getUrl, headUrl, probablyUrl, maybeIO, eitherToMaybe)

plugin :: B.Plugin
plugin = B.genPlugin "urltitle: fetches titles" loop ()

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
          maybeTitle <- getTitle url
          maybeIO sayTitle maybeTitle
        sayTitle = B.say actq chan . ("title: " ++ )

getTitle :: String -> IO (Maybe String)
getTitle url = do
    okay <- okayToGet url
    case okay of
      False -> return Nothing
      True -> do
        e <- try $ getUrl url :: IO (Either SomeException String)
        return (eitherToMaybe e >>=
                listToMaybe . sections (~== "<title>") . parseTags >>=
                fromTitle)
  where fromTitle ((TagOpen "title" []) : (TagText t) : _) = Just t
        fromTitle _ = Nothing

okayToGet :: String -> IO Bool
okayToGet url = headUrl url >>= return . headersOkay

-- Always require Content-Type to exist and start with "text". If
-- Content-Length exists, limit it to 1mb.
headersOkay :: [Header] -> Bool
headersOkay hs = lengthOkay && contentOkay
  where lengthOkay = isNothing len || (read (fromJust len) :: Integer) < oneMb
        contentOkay = isJust content && "text" `isPrefixOf` fromJust content
        oneMb = 10^(6::Integer)
        len = getHdr HdrContentLength hs
        content = getHdr HdrContentType hs
        getHdr hdr headers = -- kind of bad
            case find (\(Header name _) -> name == hdr) headers of
              Nothing -> Nothing
              Just (Header _ val) -> Just val
