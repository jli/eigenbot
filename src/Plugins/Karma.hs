module Plugins.Karma (plugin) where

import Control.Monad.State.Strict (get, put)
import Data.List (isSuffixOf, nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import System.FilePath ((</>))
import Text.Printf (printf)

import qualified Base as B
import Util (concatComma, io, mcoin, maybeM, plural)

type Points = Map B.Nick Integer
--type Reasons = Map B.Nick [String]
--type KarmaState = (Points, Reasons)

initState :: Points
initState = M.empty

stateFile :: FilePath
stateFile = B.dotDir </> "karma.state"

plugin :: B.Plugin
plugin = B.genPlugin
           "karma: !karma nick, !recent nick"
           loop
           initState
           (Just stateFile)

data Update = Up B.Nick
            | Down B.Nick
              deriving (Show)

nickOfUpdate :: Update -> B.Nick
nickOfUpdate (Up n) = n
nickOfUpdate (Down n) = n

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (max 0 ((length xs) - n)) xs

scoreNick :: String -> Maybe Update
scoreNick "++" = Nothing
scoreNick "--" = Nothing
scoreNick word | "++" `isSuffixOf` word = Just $ Up $ dropLast 2 word
               | "--" `isSuffixOf` word = Just $ Down $ dropLast 2 word
               | otherwise = Nothing

-- FIXME make standard?
type SayFun = String -> IO ()

loop :: B.PluginLoop Points
loop evq actq = do
    ev <- io $ B.readEvent evq
    case ev of
      B.ChannelMsg chan nick msg -> handleChanMsg chan nick msg
      B.PrivMsg net nick msg -> handlePrivMsg net nick msg
      _ -> mcoin
  where handleChanMsg chan _sendingNick msg =
          let say = B.say actq chan in
          case words msg of
            ["!karma"] -> printAll say
            ["!karma", one] -> printOne say one
            "!karma":many -> printPoints say many
            other -> updateAndSay say other
        printOne say nick = do
          points <- get
          io $ say $ nickPointsStr points nick
        printPoints say nicks = do
          points <- get
          case nicksPointsStr points nicks of
            "" -> mcoin
            nonempty -> io $ say nonempty
        printAll say = do
          points <- get
          io $ say $ showAll points
        handlePrivMsg _net _nick msg =
          -- no isRoot check on nick. rootmap is in IrcState!
          -- not so bad - should always be okay to snapshot
          case words msg of
            "!state":_ -> do
              points <- get
              io $ writeFile stateFile $ show points
            _ -> mcoin

updateAndSay :: SayFun -> [String] -> B.PluginStateT Points
updateAndSay say strs = do
    oldPoints <- get
    let updates = mapMaybe scoreNick strs
        changed = nub $ map nickOfUpdate updates
        newPoints = foldr updatePoints oldPoints updates
    io $ maybeM say (announce newPoints changed)
    put newPoints
  where updatePoints (Up nick) = M.insertWith' (+) nick 1
        updatePoints (Down nick) = M.insertWith' (+) nick (-1)
        announce _points [] = Nothing
        announce points nicks =
         let changeStrs = map (\n -> nickPointsStr points n) nicks in
         Just $ concatComma changeStrs

nickPointsStr :: Points -> B.Nick -> String
nickPointsStr points nick =
    case M.lookup nick points of
      Nothing -> printf "%s has 0 points" nick
      Just p -> printf "%s has %s" nick $ plural "point" p

nicksPointsStr :: Points -> [String] -> String
nicksPointsStr points nicks = concatComma $ map (nickPointsStr points) realNicks
  where realNicks = [ nick | nick <- nicks, M.member nick points ]

-- stringify entire points map. limit?
showAll :: Points -> String
showAll points =
    case M.toList points of
      [] -> "No one has any points!"
      list -> foldr buildString "" list
  where buildString (n, p) "" = printf "%s %d" n p
        buildString (n, p) str = printf "%s %d, " n p ++ str
