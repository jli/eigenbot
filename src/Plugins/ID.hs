module Plugins.ID (go) where

import Base
import Control.Monad (forever, when)
import Control.Concurrent.Chan (readChan)
import Text.Printf (printf)

go :: Plugin
go evq actq = forever $ loop
  where loop = do
          NetEvent net ev <- readChan evq
          case ev of
            ChannelMsg chan nick msg ->
              let (cmd, rest) = breakOnSpace msg
                  reply = printf "%s said:%s" nick rest
                  act = DoChannelMsg chan reply in
              when (cmd == "!id" && notMe nick) $
                addAct actq (NetAction net act)
            _ -> return ()
