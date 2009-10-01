module Plugin (
    Plugin (..)
  , Loop
  , runPlugin
) where

import Control.Concurrent.Chan (Chan)
import Control.Monad (forever)
import Control.Monad.State.Strict (StateT, runStateT)
import Text.Printf (printf)

import Base (NetAction, NetEvent)

type PluginStateT s = StateT s IO ()
type Loop s = Chan NetEvent -> Chan NetAction -> PluginStateT s

data Plugin s = Plugin {
      plugName :: String
    , plugProvides :: String
    , plugLoop :: Loop s
    , plugInitState :: s
}

runPlugin :: Plugin a -> Chan NetEvent -> Chan NetAction -> IO ()
runPlugin (Plugin name provides loop initState) evq actq = do
    printf "starting plugin %s: %s" name provides
    runStateT (forever $ loop evq actq) initState
    return ()
