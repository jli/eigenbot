-- Pull RSS feed for Github commits every so often.
-- !subscribe user repo adds the repo to internal state for polling.
-- !last n user repo shows last n commits for repo.

-- SERIOUS ISSUES:
-- - messages not implemented (only does eigenbot)
-- - non-configurable destination (hardcoded for #z)
-- - hacked up parseGithubTime / SimpleTime

module Plugins.Github (plugin) where

import Control.Monad (foldM, forM_, mapM_, when)
import Control.Monad.State.Strict (get, put)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, listToMaybe)
import Text.Printf (printf)

import Text.Feed.Import (parseFeedString)
import Text.Feed.Types (Feed(..))
import qualified Text.Atom.Feed as Atom

import qualified Base as B
import Util (io, delay, simpleHttp)

type User = String
type Project = String
type Url = String

data Repo = Repo User Project
          deriving (Eq, Ord)

instance Show Repo where
    show (Repo u p) = printf "%s/%s" u p

type RepoCommits = Map Repo [Commit]

data Commit = Commit {
      _cTitle :: String
    , _cAuthor :: String
    , cTime :: String
}

instance Show Commit where
    show (Commit title _author _time) = title

data GithubState = St {
      _ghSubscribed :: [Repo]
    , _ghJustStarted :: Bool -- FIXME should be per-channel
    , _ghCommits :: RepoCommits
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
    newCommits <- io $ fetchAll subscribed
    io $ when (not justStarted) $ announceNew actq newCommits commits
    put $ St subscribed False newCommits
    io $ delay 120 -- make configurable

fetchAll :: [Repo] -> IO (RepoCommits)
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

-- don't use fromJust!!! Maybe [Commit]!!!
pageToCommits :: String -> [Commit]
pageToCommits = map entryToCommit . Atom.feedEntries . fromJust . parseAtomString

entryToCommit :: Atom.Entry -> Commit
entryToCommit entry = Commit title author time
  where author = Atom.personName $ head $ Atom.entryAuthors entry
        time = Atom.entryUpdated entry
        title = Atom.txtToString $ Atom.entryTitle entry

announceNew :: B.ActQ -> RepoCommits -> RepoCommits -> IO ()
announceNew actq newMap oldMap = mapM_ announce newCommits
  where newCommits = M.toList $ dropOld diffCtime oldMap newMap
        diffCtime = (/=) `on` cTime
        announce (repo, newC) =
            forM_ (reverse newC)
              (\n -> toChan $ printf "new on %s: %s" (show repo) (show n))
        --toChan = B.pm actq "sigil" "jli"
        toChan = B.say actq (B.Channel "sigil" "#z")

-- 'dropOld keepFn old new' applies keepFn with the first entry of old
-- to all the entries of new, continuing until keepFn returns false.
-- This means items closer to the front of the list are considered
-- "newer".
dropOld :: Ord k => (v -> v -> Bool) -> Map k [v] -> Map k [v] -> Map k [v]
dropOld keepFn oldMap newMap = foldr removeOld newMap (M.toList oldMap)
  where removeOld (repo, oldCommits) current =
          case (do lastOld <- listToMaybe oldCommits
                   allNew <- M.lookup repo current
                   return $ takeWhile (keepFn lastOld) allNew) of
            Nothing -> current
            Just reallyNew -> M.insert repo reallyNew current
