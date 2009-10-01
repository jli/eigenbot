module Plugins.Pong (go) where

import Base
import Control.Concurrent.Chan (readChan)
import Control.Monad (forever)

go :: Plugin
go evq actq = forever $ loop
  where loop = do
          NetEvent net ev <- readChan evq
          case ev of
            Ping msg -> addAct actq (NetAction net $ DoPong msg)
            _ -> return ()
