module Main (main) where

import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as HSL

$(deriveLoggers "HSL" [HSL.DEBUG])

main :: IO ()
main = do
    HSL.updateGlobalLogger "" (HSL.setLevel HSL.DEBUG)
    debugM "eigenbot starting up"
