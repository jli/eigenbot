module Main (main) where

import System.Cmd (system)

import Distribution.Simple (defaultMainWithHooks, runTests, simpleUserHooks)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { runTests = runTests' } where
  runTests' _ _ _ _ = system "runhaskell -i./src test/*.hs" >> return ()
