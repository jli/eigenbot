module Base (
    breakOnSpace
  , notMe
  , addAction
  , readEvent
  , say
  , pm
  , runIrc
  , setup
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
  , Net (..)
  , Srv (..)
  , Channel (..)
  , Nick
  , ChannelName
  , NetName
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Monad (forever, forM_, foldM)
import Control.Monad.State.Strict (StateT, runStateT, get)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import System.IO (Handle, hGetLine, hPutStr, hSetBuffering, BufferMode (..))
import Text.Printf (printf)

import qualified Network.IRC.Base as IRC
import qualified Network.IRC.Parser as IRC
import Network (PortID(..), PortNumber, connectTo)



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

-- bleh, same as Event but with fewer params. how about just use
-- Event, but fill in Nick field with appropriate data from actq
-- handler?
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
newActq :: IO ActQ
newEvq = return . EvQ =<< newChan
newActq = return . ActQ =<< newChan

addEvent :: EvQ -> Event -> IO ()
addAction :: ActQ -> Action -> IO ()
addEvent (EvQ c) = writeChan c
addAction (ActQ c) = writeChan c

readEvent :: EvQ -> IO Event
readAction :: ActQ -> IO Action
readEvent (EvQ c) = readChan c
readAction (ActQ c) = readChan c

writeEvent :: EvQ -> Event -> IO ()
writeEvent (EvQ c) = writeChan c

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

breakOnSpace :: String -> (String, String)
breakOnSpace = break (== ' ')

notMe, isMe :: Nick -> Bool
notMe = (/= me)
isMe = (== me)

say :: ActQ -> Channel -> String -> IO ()
say actq chan msg = addAction actq $ DoChannelMsg chan msg

pm :: ActQ -> NetName -> Nick -> String -> IO ()
pm actq net nick msg = addAction actq $ DoPrivMsg net nick msg

-- needs shorter name
(+++) :: String -> String -> String
"" +++ "" = ""
s1 +++ "" = s1
"" +++ s2 = s2
s1 +++ s2 = s1 ++ (' ':s2)

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

-- NEEDS VERIFICATION
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

setup :: Irc ()
setup = do
    IS nets chans plugins evq actq <- get
    liftIO $ do
    -- connect to networks, hook up incoming events to evq through
    -- network parser, start actq handler for dispatching actions to
    -- networks
      putStrLn "setup: connecting to nets"
      connectNets nets evq actq
    -- start plugins with evq. all plugins get all events. all plugins
    -- can place actions on acq. all plugins can maintain internal
    -- state
      putStrLn "setup: starting plugins"
      startPlugins plugins evq actq
    -- join chans
      putStrLn "setup: joining chans"
      joinChans chans actq
      putStrLn "setup: done!"

startPlugins :: [Plugin] -> EvQ -> ActQ -> IO ()
startPlugins plugs evq actq = forM_ plugs startPlug
    where startPlug p = do
            evq' <- dupEvq evq
            forkIO $ runPlugin p evq' actq

joinChans :: [Channel] -> ActQ -> IO ()
joinChans chans actq =
    forM_ chans $ addAction actq . DoJoin

connectNets :: [Net] -> EvQ -> ActQ -> IO ()
connectNets nets evq actq = do
    handleMap <- buildHandleMap nets
    forkEvqAggregator handleMap
    forkActqHandler handleMap
    initConnects handleMap

  where buildHandleMap = foldM insertNetHandle M.empty
        insertNetHandle _   (Net name []) = error ("No servers for"+++name)
        insertNetHandle handleMap (Net name ((Srv srv port):_rest)) = do
          handle <- connectTo srv (PortNumber port)
          printf "connectNets: connected to %s via %s!\n" name srv
          hSetBuffering handle NoBuffering
          return $ M.insert name handle handleMap

        forkEvqAggregator handleMap =
          forM_ (M.assocs handleMap)
            (\(net, handle) -> forkIO $ forever $ parseNetCon net handle evq)

        forkActqHandler handleMap = forkIO $ forever $ handleAction handleMap
        handleAction handleMap = do
          act <- readAction actq
          let net = actionToNet act
          case M.lookup net handleMap of
            Nothing -> printf "ERROR: no handle for net %s\n" net -- uhh....
            Just h -> do
              printf "handleAction: %s: sending <%s>\n" net (actionToMsg act)
              sendAction h act

        initConnects handleMap =
          forM_ (M.keys handleMap) $ \net -> addAction actq (DoInit net me)

parseNetCon :: NetName -> Handle -> EvQ -> IO ()
parseNetCon net h evq = do
    line <- hGetLine h
    printf "parseNetCon: %s: <%s>\n" net line
    case IRC.decode line of
      Nothing -> putStrLn "parseNetCon failed a parse!" -- FIXME debug error
      Just msg -> do
        -- FIXME really need that hslogger stuff
        --printf "     parse: IRCmsg: <%s>\n" (show msg)
        --printf "     parse:  event: <%s>\n" (show event)
        maybe (return ()) (writeEvent evq) (parseEvent net msg)

sendAction :: Handle -> Action -> IO ()
sendAction h act = hPutStr h $ actionToMsg act

runIrc :: Irc () -> IrcState -> IO ()
runIrc irc initState = do
  runStateT irc initState
  return ()
