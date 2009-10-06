module Plugins.Simple (plugin) where

import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict (get, put)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import System.Time (getClockTime)
import System.Random (StdGen, next, mkStdGen)
import Text.Printf (printf)

import qualified Base as B
import Util (breakOnSpace, safeIndex)

type RandState = StdGen

initState :: RandState
initState = mkStdGen 1337

plugin :: B.Plugin
plugin = B.genPlugin "simple plugin: !id, !tell, !date" loop initState

loop :: B.PluginLoop RandState
loop evq actq = do
    ev <- liftIO $ B.readEvent evq
    case ev of
      B.ChannelMsg chan nick msg -> handleChanMsg chan nick msg
      _ -> return ()
  where handleChanMsg chan nick msg =
          let (cmd, rest) = breakOnSpace msg
              say = liftIO . B.say actq chan in
          case cmd of
            "!id" -> say $ printf "%s said:%s" nick rest
            "!tell" ->
              case words rest of
                [] -> say $ printf "TRYING TO PICK A FIGHT %s YO!" nick
                [user] -> say $ printf "hey %s, THE WORLD IS A VAMPIRE" user
                user:rest' -> say $ printf "hey %s, %s" user $ unwords rest'
            "!date" -> liftIO getClockTime >>= say . printf "date is: %s" . show
            _ -> do
              gen <- get
              case cuteness nick msg gen of
                Nothing -> return ()
                Just (cuteMsg, gen') -> do
                  say cuteMsg
                  put gen'

-- FIXME bleh. must be nicer way to wrap up state access?
cuteness :: B.Nick -> String -> StdGen -> Maybe (String, StdGen)
cuteness nick msg gen =
    if not mentionedMe
    then Nothing
    else Just msgAndNewGen
  where mentionedMe = isJust $ find (== B.me) $ words msg
        msgAndNewGen =
            let (i, gen') = next gen
                formatFn = fromJust $ safeIndex cuteFormats i
            in (formatFn nick, gen')

cuteFormats :: [B.Nick -> String]
cuteFormats =
    [ printf "hey %s ;) ;) ;)"
    , const "YO WHAT"
    , const "*hugs*"
    , const "*kisses*"
    , const "bitch if it ain't strongly typed then i ain't got the time"
    , printf "%s mothafucka get that errorprone api outta my face"
    , const "huff puff"
    , printf "%s <3 <3 <3"
    , const "I am tired of Earth, these people. I am tired of being caught in the tangle of their lives."
    , const "dog carcass in alley this morning tire tread on burst stomach"
    , printf "omg %s omg <3"
    , const "(.).(.) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c"
    ]
