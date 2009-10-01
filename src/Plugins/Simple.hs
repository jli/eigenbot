module Plugins.Simple (plugin) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Chan (readChan)
import System.Time (getClockTime)
import Text.Printf (printf)

import qualified Base as B

-- This simple plugin has () as state.

plugin :: B.Plugin
plugin = B.genPlugin "simple plugin: !id, !tell, !date" loop ()

loop :: B.PluginLoop ()
loop evq actq = liftIO $ do
    B.NetEvent net ev <- readChan evq
    case ev of
      B.ChannelMsg chan nick msg -> do
        let (cmd, rest) = B.breakOnSpace msg
        maybeAction <-
              case cmd of
                "!id" ->
                  let reply = printf "%s said:%s" nick rest
                  in return $ Just $ B.DoChannelMsg chan reply
                "!tell" ->
                  case words rest of
                    user:rest' ->
                      let reply = printf "hey %s, %s" user $ unwords rest'
                      in return $ Just $ B.DoChannelMsg chan reply
                    _ -> return Nothing
                "!date" -> do
                  clockTime <- getClockTime
                  let reply = printf "date is: %s" $ show clockTime
                  return $ Just $ B.DoChannelMsg chan reply
                _ -> return Nothing
        case maybeAction of
          Nothing -> return ()
          Just action ->
              when (B.notMe nick) $ B.addAct actq (B.NetAction net action)
      _ -> return ()