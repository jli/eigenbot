module Main (main) where

import Control.Monad (liftM2)
import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as HSL

import qualified Base as B
import qualified Plugins.Pong
import qualified Plugins.Simple
import qualified Plugins.Postpone
import qualified Plugins.Karma
import qualified Plugins.Github
import qualified Plugins.UrlTitle

$(deriveLoggers "HSL" [HSL.DEBUG])

hardCodedState :: IO B.IrcState
hardCodedState = do
    liftM2 (\ev act -> B.IS nets chans plugs ev act)
           B.newEvq B.newActq
  where nets = [B.Net "sigil" [B.Srv "sigil.yi.org" 56667]]
        chans = [B.Channel "sigil" "#t", B.Channel "sigil" "#z"]
        plugs = [ Plugins.Pong.plugin
                , Plugins.Simple.plugin
                , Plugins.Postpone.plugin
                , Plugins.Karma.plugin
                , Plugins.Github.plugin
                , Plugins.UrlTitle.plugin
                ]

main :: IO ()
main = do
    HSL.updateGlobalLogger "" (HSL.setLevel HSL.DEBUG)
    debugM "eigenbot starting up"
    initState <- hardCodedState
    B.runIrc B.setup initState
    return ()
