module Main2 (main) where

import Control.Concurrent.Chan (newChan)
import Control.Monad (liftM2)

import qualified Base as B
import qualified Plugins.Pong
import qualified Plugins.Simple

hardCodedState :: IO B.IrcState
hardCodedState = do
    liftM2 (\ev act -> B.IS nets chans plugs ev act)
           newChan newChan
  where nets = [B.Net "sigil" [B.Srv "sigil.yi.org" 56667]]
        chans = [B.Channel "sigil" "#t", B.Channel "sigil" "#z"]
        plugs = [Plugins.Pong.plugin, Plugins.Simple.plugin]

main :: IO ()
main = do
    initState <- hardCodedState
    B.runIrc B.setup initState
    return ()
