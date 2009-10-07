module Plugins.Karma (plugin) where

import Control.Monad (mplus)
import Control.Monad.State.Strict (get, put)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

import qualified Base as B
import Util (io, mcoin, maybeIO, plural)

type Points = Map B.Nick Integer
--type Reasons = Map B.Nick [String]
--type KarmaState = (Points, Reasons)

initState :: Points
initState = M.empty

plugin :: B.Plugin
plugin = B.genPlugin
           "karma: !karma nick, !recent nick"
           loop
           initState

data Update = Up B.Nick
            | Down B.Nick

nickOfUpdate :: Update -> B.Nick
nickOfUpdate (Up n) = n
nickOfUpdate (Down n) = n

scoreNickPre, scoreNick :: String -> Maybe Update
scoreNickPre ('+':'+':[]) = Nothing
scoreNickPre ('-':'-':[]) = Nothing
scoreNickPre ('+':'+':nick) = Just $ Up nick
scoreNickPre ('-':'-':nick) = Just $ Down nick
scoreNickPre _ = Nothing

reverseUpdate :: Update -> Update
reverseUpdate (Up s) = Up $ reverse s
reverseUpdate (Down s) = Down $ reverse s

scoreNick word = scoreNickPre word `mplus` maybeReversed
  where maybeReversed =
          fmap reverseUpdate (scoreNickPre $ reverse word)

-- FIXME make standard?
type SayFun = String -> IO ()

loop :: B.PluginLoop Points
loop evq actq = do
    ev <- io $ B.readEvent evq
    case ev of
      B.ChannelMsg chan nick msg -> handleChanMsg chan nick msg
      _ -> mcoin
  where handleChanMsg chan _sendingNick msg =
          let say = B.say actq chan in
          case words msg of
            "!karma":nick:_ -> printPoints say nick
            ["!karma"] -> printAll say
            rest -> updateAndSay say rest
        printPoints say nick = do
          points <- get
          io $ say $ nickPointsStr points nick
        printAll say = do
          points <- get
          io $ say $ showAll points

updateAndSay :: SayFun -> [String] -> B.PluginStateT Points
updateAndSay say strs = do
    oldPoints <- get
    let updates = mapMaybe scoreNick strs
        changed = nub $ map nickOfUpdate updates
        newPoints = foldr updatePoints oldPoints updates
    io $ maybeIO say (announce newPoints changed)
    put newPoints
  where updatePoints (Up nick) = M.insertWith' (+) nick (1)
        updatePoints (Down nick) = M.insertWith' (+) nick (-1)
        announce _points [] = Nothing
        announce points nicks =
         let changeStrs = map (\n -> nickPointsStr points n) nicks in
         Just $ foldr buildString "" changeStrs
        buildString str "" = str
        buildString str rest = str ++ ", " ++ rest

nickPointsStr :: Points -> B.Nick -> String
nickPointsStr points nick =
    case M.lookup nick points of
      Nothing -> printf "%s has 0 points" nick
      Just p -> printf "%s has %s" nick $ plural "point" p

-- stringify entire points map. limit?
showAll :: Points -> String
showAll points =
    case M.toList points of
      [] -> "No one has any points!"
      list -> foldr buildString "" list
  where buildString (n, p) "" = printf "%s %d" n p
        buildString (n, p) str = printf "%s %d, " n p ++ str
