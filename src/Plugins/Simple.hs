module Plugins.Simple (plugin) where

import Data.Char (toLower)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust, isJust)
import System.Time (getClockTime)
import System.Random (randomIO)
import Text.Printf (printf)

import qualified Base as B
import Util ((<||>), io, mcoin, breakOnSpace, safeIndex)

plugin :: B.Plugin
plugin = B.genPlugin "simple plugin: !id, !tell, !date" loop () Nothing

loop :: B.PluginLoop ()
loop evq actq = do
    ev <- io $ B.readEvent evq
    case ev of
      B.ChannelMsg chan nick msg -> handleChanMsg chan nick msg
      _ -> mcoin
  where handleChanMsg chan nick msg =
          let (cmd, rest) = breakOnSpace msg
              say = io . B.say actq chan in
          case cmd of
            "!id" -> say $ printf "%s said:%s" nick rest
            "!tell" ->
              case words rest of
                [] -> say $ printf "TRYING TO PICK A FIGHT %s YO!" nick
                [user] -> say $ printf "hey %s, THE WORLD IS A VAMPIRE" user
                user:rest' -> say $ printf "hey %s, %s" user $ unwords rest'
            "!date" -> io getClockTime >>= say . printf "date is: %s" . show
            _ ->
              case cuteness nick msg of
                Nothing -> mcoin
                Just cuteMsg -> io cuteMsg >>= say

cuteness :: B.Nick -> String -> Maybe (IO String)
cuteness nick msg =
    if not mentionedMe
    then Nothing
    else Just cute
  where mentionedMe = isJust $ find includesMe $ words msg
        includesMe = ((B.me `isPrefixOf`) <||> (B.me `isSuffixOf`)) . lower
        lower = map toLower
        cute = do
          i <- randomIO
          let formatFn = fromJust $ safeIndex cuteFormats i
          return $ formatFn nick

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
