-- improvements:
-- - ignore non-html pages (in particular, avoid fetching large binary files)
-- - filter out URLs to prevent abuse
-- - only allow port 80?

module Plugins.UrlTitle (plugin) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (mapM_)
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, maybe)

import Network.Browser (browse, setAllowRedirects, setMaxRedirects, request)
import Network.HTTP (getRequest, getResponseBody)
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
fetchUrl :: String -> IO String
fetchUrl url = do
      (_, rsp) <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               setMaxRedirects $ Just 5
               request $ getRequest url
      getResponseBody $ Right rsp


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
    e <- try $ fetchUrl url :: IO (Either SomeException String)
    return (eitherToMaybe e >>=
            listToMaybe . sections (~== "<title>") . parseTags >>=
            fromTitle)
  where fromTitle ((TagOpen "title" []) : (TagText t) : _) = Just t
        fromTitle _ = Nothing
