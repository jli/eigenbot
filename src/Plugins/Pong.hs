module Plugins.Pong (plugin) where

import Control.Concurrent.Chan (readChan)
import Control.Monad.Trans (liftIO)

import qualified Base as B

plugin :: B.Plugin
plugin = B.genPlugin "pong: pongs to pings" loop ()

loop :: B.PluginLoop ()
loop evq actq = liftIO $ do
    B.NetEvent net ev <- readChan evq
    case ev of
      B.Ping msg -> B.addAct actq (B.NetAction net $ B.DoPong msg)
      _ -> return ()
