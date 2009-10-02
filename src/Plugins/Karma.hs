module Plugins.Karma (plugin) where


import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict (get, modify)
import Control.Concurrent.Chan (readChan)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

import qualified Base as B

type Points = Map B.Nick Int
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

scoreNickPre, scoreNick :: String -> Maybe Update
scoreNickPre ('+':'+':[]) = Nothing
scoreNickPre ('+':'+':nick) = Just $ Up nick
scoreNickPre ('-':'-':[]) = Nothing
scoreNickPre ('-':'-':nick) = Just $ Down nick
scoreNickPre _ = Nothing

-- YUCK! not sure how to make this better
scoreNick word =
    case scoreNickPre word of
      update@(Just _) -> update
      Nothing ->
        case scoreNickPre $ reverse word of
          Just (Up nick) -> Just (Up $ reverse nick)
          Just (Down nick) -> Just (Down $ reverse nick)
          Nothing -> Nothing

loop :: B.PluginLoop Points
loop evq actq = do
    B.NetEvent net ev <- liftIO $ readChan evq
    case ev of
      B.ChannelMsg chan nick msg -> handleChanMsg net chan nick msg
      _ -> return ()
  where handleChanMsg net chan _sendNick msg =
          case words msg of
            "!karma":nick:_ -> printPoints net chan nick
            rest -> update rest msg
        printPoints net chan nick = do
            points <- get
            let reply = case M.lookup nick points of
                          Nothing -> printf "%s has 0 points" nick
                          Just p -> printf "%s has %d points" nick p
            liftIO $ B.say actq net chan reply
        update msgWords _origMsg =
            updatePoints $ mapMaybe scoreNick msgWords

updatePoints :: [Update] -> B.PluginStateT Points
updatePoints updates =
    modify (\points -> foldr updateMap points updates)
  where updateMap (Up nick) origPoints =
            M.insertWith' (+) nick 1 origPoints
        updateMap (Down nick) origPoints =
            M.insertWith' (+) nick (-1) origPoints
