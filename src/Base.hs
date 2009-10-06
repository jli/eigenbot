module Base (
    me
  , notMe
  , addAction
  , readEvent
  , say
  , pm
  , runBot
  , newActq
  , newEvq
  , ActQ
  , Event (..)
  , Action (..)
  , IrcState (..)
  , Plugin
  , PluginStateT
  , PluginLoop
  , genPlugin
  , Nick
  , Net (..)
  , Srv (..)
  , Channel (..)
  , NetName
  , ChannelName
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Monad (forever, forM_, foldM, liftM)
import Control.Monad.State.Strict (StateT, runStateT, get)
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO (Handle, hGetLine, hPutStr, hSetBuffering, BufferMode (..))
import Text.Printf (printf)

import qualified Network.IRC.Base as IRC
import qualified Network.IRC.Parser as IRC
import Network (PortID(..), PortNumber, connectTo)

import Util((+++), doForever, dropNewlines, maybeIO)


type Nick = String
type NetName = String
type SrvName = String
type ChannelName = String

data Srv = Srv SrvName PortNumber
data Channel = Channel NetName ChannelName
             deriving (Eq, Ord, Show)
-- has enough data to allow connections. multiple servers allow
-- redundant connections
data Net = Net NetName [Srv]

data Event = Join Channel Nick
           | Part Channel Nick String
           | ChannelMsg Channel Nick String
           | PrivMsg NetName Nick String
           | Ping NetName String
             deriving (Show)

data Action = DoJoin Channel
            | DoPart Channel String
            | DoChannelMsg Channel String
            | DoPrivMsg NetName Nick String
            | DoInit NetName Nick
            | DoPong NetName String

-- action and event queues
newtype EvQ = EvQ (Chan Event)
newtype ActQ = ActQ (Chan Action)

newEvq :: IO EvQ
newEvq = EvQ `liftM` newChan

newActq :: IO ActQ
newActq = ActQ `liftM` newChan

readEvent :: EvQ -> IO Event
readEvent (EvQ c) = readChan c

readAction :: ActQ -> IO Action
readAction (ActQ c) = readChan c

addEvent :: EvQ -> Event -> IO ()
addEvent (EvQ c) = writeChan c

addAction :: ActQ -> Action -> IO ()
addAction (ActQ c) = writeChan c

dupEvq :: EvQ -> IO EvQ
dupEvq (EvQ c) = return . EvQ =<< dupChan c

-- kind of gross
type PluginStateT s = StateT s IO ()
type PluginLoop s = EvQ -> ActQ -> PluginStateT s
type Plugin = EvQ -> ActQ -> IO ()

genPlugin :: String -> PluginLoop s -> s -> Plugin
genPlugin description loop initState =
    \evq actq -> do
       printf "plugin starting: %s\n" description
       runStateT (forever $ loop evq actq) initState
       return ()

runPlugin :: Plugin -> EvQ -> ActQ -> IO ()
runPlugin plug evq actq = plug evq actq

-- needs to track connection status, needs requestedConnects.
-- a main loop should track, do actions, and update appropriately.
-- how to communicate between main loop and plugins? another chan?
-- or maybe everything that changes this state should live in main loop?
data IrcState = IS {
      stNets :: [Net] -- networks to always connect to
    , stChannels :: [Channel]
    , stPlugins :: [Plugin]
    , stEvq :: EvQ
    , stAcq :: ActQ
}

type Irc a = StateT IrcState IO a



-- make this changeable?
me :: Nick
me = "eigenbot"

notMe, isMe :: Nick -> Bool
notMe = (/= me)
isMe = (== me)

say :: ActQ -> Channel -> String -> IO ()
say actq chan msg = addAction actq $ DoChannelMsg chan msg

pm :: ActQ -> NetName -> Nick -> String -> IO ()
pm actq net nick msg = addAction actq $ DoPrivMsg net nick msg

type IrcCmd = String

genMsg, genMsgColon :: IrcCmd -> String -> String
genMsg cmd rest = printf "%s %s\r\n" cmd rest
genMsgColon cmd rest = genMsg cmd (':' : rest)

actionToNet :: Action -> NetName
actionToNet (DoJoin (Channel n _)) = n
actionToNet (DoPart (Channel n _) _msg) = n
actionToNet (DoChannelMsg (Channel n _) _msg) = n
actionToNet (DoPrivMsg n _nick _msg) = n
actionToNet (DoInit n _nick) = n
actionToNet (DoPong n _msg) = n

-- use irc's Message?
actionToMsg :: Action -> String
actionToMsg (DoJoin (Channel _ chan)) = genMsg "JOIN" chan
actionToMsg (DoPart (Channel _ chan) msg) = genMsgColon ("PART"+++chan) msg
actionToMsg (DoChannelMsg (Channel _ chan) msg) = genMsgColon ("PRIVMSG"+++chan) msg
actionToMsg (DoPrivMsg _net nick msg) = genMsgColon ("PRIVMSG"+++nick) msg
actionToMsg (DoInit _net nick) = genMsg "NICK" nick ++ genMsg "USER" (nick ++ " 0 * :realname")
actionToMsg (DoPong _net str) = genMsgColon "PONG" str

-- FIXME NEEDS VERIFICATION
parseEvent :: NetName -> IRC.Message -> Maybe Event
parseEvent net (IRC.Message (Just (IRC.NickName nick _user _server)) cmd params) =
    case cmd of
      "JOIN" -> Just $ Join (Channel net (params !! 0)) nick
      "PART" -> Just $ Part (Channel net (params !! 0)) nick (params !! 1)
      "PRIVMSG" ->
        let toPart = params !! 0
            rest = unwords $ drop 1 params in
        if isMe toPart
        then Just $ PrivMsg net toPart rest
        else Just $ ChannelMsg (Channel net toPart) nick rest
      _ -> Nothing
parseEvent _net (IRC.Message (Just (IRC.Server _name)) _ _) = Nothing
parseEvent net (IRC.Message Nothing cmd params) =
    case cmd of
      "PING" -> Just $ Ping net (params !! 0)
      _ -> Nothing

-- needs some hslogger love
-- need to move comments from here to some fancy architecture overview
setup :: Irc ()
setup = do
    IS nets chans plugins evq actq <- get
    liftIO $ do
      -- hook up incoming events to evq through network parser, start
      -- actq handler for dispatching actions to networks
      connectNets nets evq actq
      -- all plugins get all events
      startPlugins plugins evq actq
      joinChans chans actq
    --listenLoop

connectNets :: [Net] -> EvQ -> ActQ -> IO ()
connectNets nets evq actq = do
    handleMap <- buildHandleMap nets
    forkNetHandlers handleMap
    forkActionHandler handleMap
    initConnects handleMap

  where buildHandleMap = foldM insertHandle M.empty
        insertHandle _         (Net name []) = error $ "No servers for"+++name
        insertHandle handleMap (Net name ((Srv srv port):_rest)) = do
          handle <- connectTo srv (PortNumber port)
          hSetBuffering handle LineBuffering
          return $ M.insert name handle handleMap

        forkNetHandlers handleMap =
          forM_ (M.assocs handleMap)
            (\(net, handle) -> doForever $ netHandler net handle evq)

        -- need more complicated handling of handleMap to get dynamic
        -- network connections. needs to be changeable by main loop
        -- and accessible by actionHandler.
        forkActionHandler handleMap = doForever $ actionHandler handleMap actq

        initConnects handleMap =
          forM_ (M.keys handleMap) $ \net -> addAction actq (DoInit net me)

-- from a network handle, parses lines into events and places them on
-- the event queue
netHandler :: NetName -> Handle -> EvQ -> IO ()
netHandler net h evq = do
    line <- hGetLine h
    printf "from %s: <%s>\n" net $ dropNewlines line
    case IRC.decode line of
      Nothing -> putStrLn "netHandler failed a parse!" -- FIXME debug error
      Just msg -> maybeIO (addEvent evq) (parseEvent net msg)

actionHandler :: Map NetName Handle -> ActQ -> IO ()
actionHandler handleMap actq = do
  act <- readAction actq
  let net = actionToNet act
  case M.lookup net handleMap of
    Nothing -> error $ printf "no handle for net %s\n" net
    Just h -> do
      printf "to %s <%s>\n" net (dropNewlines $ actionToMsg act)
      sendActionToNet h act


sendActionToNet :: Handle -> Action -> IO ()
sendActionToNet h act = hPutStr h $ actionToMsg act

startPlugins :: [Plugin] -> EvQ -> ActQ -> IO ()
startPlugins plugs evq actq = forM_ plugs startPlug
    where startPlug p = do
            -- duplicate event queue so all plugins get all events
            evq' <- dupEvq evq
            forkIO $ runPlugin p evq' actq

joinChans :: [Channel] -> ActQ -> IO ()
joinChans chans actq = forM_ chans (\c -> addAction actq $ DoJoin c)

runBot :: IrcState -> IO ()
runBot initState = do
  runStateT setup initState
  return ()
