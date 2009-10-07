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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
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
      cTitle :: String
    , _cAuthor :: String
    , _cTime :: String
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
    io $ delay 300 -- make configurable

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

pageToCommits :: String -> [Commit]
pageToCommits = map entryToCommit . Atom.feedEntries . fromJust . parseAtomString

entryToCommit :: Atom.Entry -> Commit
entryToCommit entry = Commit title author time
  where author = Atom.personName $ head $ Atom.entryAuthors entry
        time = Atom.entryUpdated entry
        title = Atom.txtToString $ Atom.entryTitle entry

-- FIXME dense and ugly
announceNew :: B.ActQ -> RepoCommits -> RepoCommits -> IO ()
announceNew actq newMap oldMap = mapM_ announce newCommits
  where newCommits = M.toList $ foldr removeOld newMap (M.toList oldMap)
        removeOld (repo, oldCommits) curMap =
            let newestOld = cTitle $ head oldCommits  -- be safer
                cur = fromJust $ M.lookup repo curMap
                keepNew = takeWhile ((/= newestOld) . cTitle) cur
            in M.insert repo keepNew curMap
        announce (repo, newC) =
            forM_ (reverse newC)
              (\n -> toZ $ printf "new on %s: %s" (show repo) (show n))
        --toJli = B.pm actq "sigil" "jli"
        toZ = B.say actq (B.Channel "sigil" "#z")
