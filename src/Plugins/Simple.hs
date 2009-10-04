module Plugins.Simple (plugin) where

import Control.Monad.Trans (liftIO)
import Data.List (find, isPrefixOf)
import Data.Maybe (isJust, maybe)
import System.Time (getClockTime)
import Text.Printf (printf)

import qualified Base as B
import Util (breakOnSpace)

-- This simple plugin has () as state.

plugin :: B.Plugin
plugin = B.genPlugin "simple plugin: !id, !tell, !date" loop ()

loop :: B.PluginLoop ()
loop evq actq = liftIO $ do
    ev <- B.readEvent evq
    case ev of
      B.ChannelMsg chan nick msg -> do
        let (cmd, rest) = breakOnSpace msg
            say = B.say actq chan
        case cmd of
          "!id" -> say $ printf "%s said:%s" nick rest
          "!tell" ->
            case words rest of
              [] -> say $ printf "TRYING TO PICK A FIGHT %s YO!" nick
              [user] -> say $ printf "hey %s, THE WORLD IS A VAMPIRE" user
              user:rest' -> say $ printf "hey %s, %s" user $ unwords rest'
          "!date" -> getClockTime >>= say . printf "date is: %s" . show
          _ -> maybe (return ()) say (cuteness nick msg)
      _ -> return ()

-- FIXME some random would be cool
cuteness :: B.Nick -> String -> Maybe String
cuteness nick msg =
    if addressedMe
    then Just $ printf "hey %s ;) ;) ;)" nick
    else if mentionedMe
    then Just $ printf "YO WHAT"
    else Nothing
  where addressedMe =
            case words msg of
              first:_ -> B.me `isPrefixOf` first
              [] -> False
        mentionedMe = isJust $ find (== B.me) $ words msg
