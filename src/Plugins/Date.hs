module Plugins.Date (go) where

import Base
import Control.Concurrent.Chan (readChan)
import Control.Monad (forever, when)
import System.Time (getClockTime)
import Text.Printf (printf)

go :: Plugin
go evq actq = forever $ loop
  where loop = do
          NetEvent net ev <- readChan evq
          case ev of
            ChannelMsg chan nick msg -> do
              clockTime <- getClockTime
              let (cmd, _rest) = breakOnSpace msg
                  reply = printf "date is: %s" $ show clockTime
                  act = DoChannelMsg chan reply
              when ("!date" == cmd && notMe nick) $
                addAct actq (NetAction net act)
            _ -> return ()
