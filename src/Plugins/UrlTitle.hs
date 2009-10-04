-- improvements:
-- - filter out URLs to prevent abuse
-- - only allow port 80?

module Plugins.UrlTitle (plugin) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (mapM_)
import Control.Monad.Trans (liftIO)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe, maybe)

import Network.Browser (browse, setAllowRedirects, setMaxRedirects, request)
import Network.HTTP (getRequest, getResponseBody)
import Network.HTTP.Base (Response(..), Request(..), RequestMethod(..))
import Network.HTTP.Headers (Header(..), HeaderName(..))
import Text.HTML.TagSoup (Tag(..), sections, (~==))
import Text.HTML.TagSoup.Parser (parseTags)

import qualified Base as B

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeIO :: (a -> IO ()) -> Maybe a -> IO ()
maybeIO f m = maybe (return ()) f m

probablyUrl :: String -> Bool
probablyUrl s =
    "http://" `isPrefixOf` s ||
    "https://" `isPrefixOf` s

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


plugin :: B.Plugin
plugin = B.genPlugin "urltitle: fetches titles" loop ()

loop :: B.PluginLoop ()
loop evq actq = do
    ev <- liftIO $ B.readEvent evq
    case ev of
      B.ChannelMsg chan _nick msg -> parseAndAnnounce actq chan $ words msg
      _ -> return ()

parseAndAnnounce :: B.ActQ -> B.Channel -> [String] -> B.PluginStateT ()
parseAndAnnounce actq chan strs =
    liftIO $ mapM_ (forkIO . announceMaybe) $ filter probablyUrl strs
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
