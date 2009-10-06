module Main (main) where

import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as HSL

import Data.Map (fromList)

import qualified Base as B
import qualified Plugins.Simple
import qualified Plugins.Postpone
import qualified Plugins.Karma
import qualified Plugins.Github
import qualified Plugins.UrlTitle

$(deriveLoggers "HSL" [HSL.DEBUG])

hardCodedState :: B.BotConfig
hardCodedState = B.BotConfig nets chans roots plugs
  where nets = [ B.Net "sigil" [B.Srv "sigil.yi.org" 56667]
               , B.Net "freenode" [B.Srv "irc.freenode.net" 6667]]
        chans = [ B.Channel "sigil" "#t"
                , B.Channel "sigil" "#z"]
        roots = fromList [("sigil", ["jli"]), ("freenode", ["jli"])]
        plugs = [ Plugins.Simple.plugin
                , Plugins.Postpone.plugin
                , Plugins.Karma.plugin
                , Plugins.Github.plugin
                , Plugins.UrlTitle.plugin
                ]

main :: IO ()
main = do
    HSL.updateGlobalLogger "" (HSL.setLevel HSL.DEBUG)
    debugM "eigenbot starting up"
    B.runBot hardCodedState
