-- Pull RSS feed for Github commits every so often.
-- !subscribe user repo adds the repo to internal state for polling.
-- !last n user repo shows last n commits for repo.

-- SERIOUS ISSUES:
-- - messages not implemented (only does eigenbot)
-- - non-configurable destination (hardcoded for #z)
-- - hacked up parseGithubTime / SimpleTime

module Plugins.Github (plugin) where

import Control.Monad (foldM, forM_, mapM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict (get, put)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (splitAt)
import Text.Printf (printf)

import Text.Feed.Import (parseFeedString)
import Text.Feed.Types (Feed(..))
import qualified Text.Atom.Feed as Atom

import qualified Base as B
import Util (appFst, delay, simpleHttp)

type User = String
type Project = String
type Url = String

data Repo = Repo User Project
          deriving (Eq, Ord)

instance Show Repo where
    show (Repo u p) = printf "%s/%s" u p

data SimpleTime = SimpleTime {
      _stYear :: Int
    , _stMonth :: Int
    , _stDay :: Int
    , _stHour :: Int
    , _stMin :: Int
    , _stSec :: Int
} deriving (Eq)

instance Show SimpleTime where
    show (SimpleTime y mo d h m s) =
        printf "%04d-%02d-%02d %02d:%02d:%02d" y mo d h m s

-- yuck
dhms :: Int -> Int -> Int -> Int -> Int
dhms d h m s = s + 60*m + 3600*h + 86400*d
instance Ord SimpleTime where
    compare (SimpleTime y1 mo1 d1 h1 m1 s1) (SimpleTime y2 mo2 d2 h2 m2 s2) =
        case compare y1 y2 of
          EQ ->
            case compare mo1 mo2 of
              EQ -> compare (dhms d1 h1 m1 s1) (dhms d2 h2 m2 s2)
              other -> other
          other -> other


data Commit = Commit {
      _cTitle :: String
    , _cAuthor :: String
    , cTime :: SimpleTime
}

instance Show Commit where
    show (Commit title author time) =
        printf "%s by %s %s" title author (show time)

data GithubState = St {
      _ghSubscribed :: [Repo]
    , _ghJustStarted :: Bool -- FIXME should be per-channel
    , _ghCommits :: Map Repo [Commit]
}

initState :: GithubState
initState = St [eigenRepo] True M.empty

rssUrl :: Repo -> Url
rssUrl (Repo user proj) =
    printf "http://github.com/feeds/%s/commits/%s/master" user proj

eigenRepo :: Repo
eigenRepo = Repo "jli" "eigenbot"

plugin :: B.Plugin
plugin = B.genPlugin
           "github: !subscribe user project, !last n user project"
           loop
           initState

-- use separate thread for polling?
loop :: B.PluginLoop GithubState
loop _evq actq = do
    St subscribed justStarted commits <- get
    newCommits <- liftIO $ fetchAll subscribed
    liftIO $ when (not justStarted) $ announceNew actq newCommits commits
    put $ St subscribed False newCommits
    liftIO $ delay 300 -- make configurable

fetchAll :: [Repo] -> IO (Map Repo [Commit])
fetchAll repos =
    foldM fetchAndAdd M.empty repos
  where fetchAndAdd commitMap repo = do
          cs <- fetchCommits repo
          return $ M.insert repo cs commitMap

fetchCommits :: Repo -> IO [Commit]
fetchCommits repo = simpleHttp (rssUrl repo) >>= return . pageToCommits

parseAtomString :: String -> Maybe Atom.Feed
parseAtomString s =
    case parseFeedString s of
      Just (AtomFeed f) -> Just f
      _ -> Nothing

pageToCommits :: String -> [Commit]
pageToCommits = map entryToCommit . Atom.feedEntries . fromJust . parseAtomString

entryToCommit :: Atom.Entry -> Commit
entryToCommit entry = Commit title author time
  where author = Atom.personName $ head $ Atom.entryAuthors entry
        time = parseGithubTime $ Atom.entryUpdated entry
        title = Atom.txtToString $ Atom.entryTitle entry

-- example: "2009-10-02T17:37:32-07:00"
-- FIXME really should get good at Parsec...
parseGithubTime :: Atom.Date -> SimpleTime
parseGithubTime ghTime =
    SimpleTime y (mo) d h m s
  where (y, t4) = appFst read $ splitAt 4 ghTime
        (mo, t7) = appFst (toEnum . (+ 1) . read) $ splitAt 2 $ tail t4
        (d, t10) = appFst read $ splitAt 2 $ tail t7
        (h, t13) = appFst read $ splitAt 2 $ tail t10
        (m, t16) = appFst read $ splitAt 2 $ tail t13
        (s, _t19) = appFst read $ splitAt 2 $ tail t16

-- FIXME dense and ugly
announceNew :: B.ActQ -> Map Repo [Commit] -> Map Repo [Commit] -> IO ()
announceNew actq newMap oldMap =
    mapM_ announce newCommits
  where newCommits = M.toList $ foldr removeOld newMap (M.toList oldMap)
        removeOld (repo, oldCommits) curMap =
            let newestOld = cTime $ head oldCommits  -- be safer
                cur = fromJust $ M.lookup repo curMap
                keepNew = takeWhile ((> newestOld) . cTime) cur
            in M.insert repo keepNew curMap
        announce (repo, newC) =
            forM_ (reverse newC)
              (\n -> toZ $ printf "new on %s: %s" (show repo) (show n))
        --toJli = B.pm actq "sigil" "jli"
        toZ = B.say actq (B.Channel "sigil" "#z")
