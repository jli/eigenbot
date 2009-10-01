module Base (
    breakOnSpace
  , notMe
  , addAct
  , runIrc
  , setup
  , Event (..)
  , Action (..)
  , NetEvent (..)
  , NetAction (..)
  , IrcState (..)
  , Plugin
  , PluginLoop
  , genPlugin
  , Net (..)
  , Srv (..)
  , Channel (..)
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan, dupChan)
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
-- has enough data to allow connections. multiple servers allow
-- redundant connections
data Net = Net NetName [Srv]

data Event = Join ChannelName Nick
           | Part ChannelName Nick String
           | ChannelMsg ChannelName Nick String
           | PrivMsg Nick String -- need nick? should always be to me... maybe "me" has different nicks?
           | Ping String
           | Noop
             deriving (Show)

-- bleh, same as Event but with fewer params. how about just use
-- Event, but fill in Nick field with appropriate data from actq
-- handler?
data Action = DoJoin ChannelName
            | DoPart ChannelName String
            | DoChannelMsg ChannelName String
            | DoPrivMsg Nick String
            | DoInit Nick
            | DoPong String

data NetEvent = NetEvent NetName Event
data NetAction = NetAction NetName Action

type EvQ = Chan NetEvent
type ActQ = Chan NetAction

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

-- should track connection status...?
data IrcState = IS {
      stNets :: [Net] -- networks to always connect to
    , stChannels :: [Channel]
    , stPlugins :: [Plugin]
    --, netCons :: Map Net Handle
    , stEvq :: EvQ
    , stAcq :: ActQ
}

type Irc a = StateT IrcState IO a



me :: Nick
me = "eigenbot"

breakOnSpace :: String -> (String, String)
breakOnSpace = break (== ' ')

notMe, isMe :: Nick -> Bool
notMe = (/= me)
isMe = (== me)

addAct :: ActQ -> NetAction -> IO ()
addAct = writeChan

(+++) :: String -> String -> String
"" +++ "" = ""
s1 +++ "" = s1
"" +++ s2 = s2
s1 +++ s2 = s1 ++ (' ':s2)

-- grr - use irc's types!
type IrcCmd = String

genMsg, genMsgColon :: IrcCmd -> String -> String
genMsg cmd rest = printf "%s %s\r\n" cmd rest
genMsgColon cmd rest = genMsg cmd (':' : rest)

-- use irc's Message?
actionToMsg :: Action -> String
actionToMsg (DoJoin chan) = genMsg "JOIN" chan
actionToMsg (DoPart chan msg) = genMsgColon ("PART"+++chan) msg
actionToMsg (DoChannelMsg chan msg) = genMsgColon ("PRIVMSG"+++chan) msg
actionToMsg (DoPrivMsg nick msg) = genMsgColon ("PRIVMSG"+++nick) msg
actionToMsg (DoInit nick) = genMsg "NICK" nick ++ genMsg "USER" (nick ++ " 0 * :realname")
actionToMsg (DoPong str) = genMsgColon "PONG" str

-- NEEDS VERIFICATION
parseEvent :: IRC.Message -> Event
parseEvent (IRC.Message (Just (IRC.NickName nick _user _server)) cmd params) =
    case cmd of
      "JOIN" -> Join (params !! 0) nick
      "PART" -> Part (params !! 0) nick (params !! 1)
      "PRIVMSG" -> let toPart = params !! 0
                       rest = unwords $ drop 1 params in
                   -- oh no! need user name list! maybe just have privmsg? :(
                   -- oh wait! privates can only be to *ME*!
                   if isMe toPart
                   then PrivMsg toPart rest
                   else ChannelMsg toPart nick rest
      _ -> Noop
parseEvent (IRC.Message (Just (IRC.Server _name)) _ _) = Noop
parseEvent (IRC.Message Nothing cmd params) =
    case cmd of
      "PING" -> Ping (params !! 0)
      _ -> Noop

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
            dupEvq <- dupChan evq
            forkIO $ runPlugin p dupEvq actq

joinChans :: [Channel] -> ActQ -> IO ()
joinChans chans actq =
    forM_ chans $
      \(Channel net chan) -> addAct actq (NetAction net (DoJoin chan))

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
          NetAction net act <- readChan actq
          case M.lookup net handleMap of
            Nothing -> printf "ERROR: no handle for net %s\n" net -- uhh....
            Just h -> do
              printf "handleAction: %s: sending <%s>\n" net (actionToMsg act)
              sendAction h act

        initConnects handleMap =
          forM_ (M.keys handleMap) $ \net -> addAct actq (NetAction net (DoInit me))

parseNetCon :: NetName -> Handle -> EvQ -> IO ()
parseNetCon net h evq = do
    line <- hGetLine h
    printf "parseNetCon: %s: <%s>\n" net line
    case IRC.decode line of
      Nothing -> putStrLn "parseNetCon failed a parse!" -- FIXME debug error
      Just msg -> do
        let event = parseEvent msg
        printf "     parse: IRCmsg: <%s>\n" (show msg)
        printf "     parse:  event: <%s>\n" (show event)
        writeChan evq $ NetEvent net event

sendAction :: Handle -> Action -> IO ()
sendAction h act = hPutStr h $ actionToMsg act

runIrc :: Irc () -> IrcState -> IO ()
runIrc irc initState = do
  runStateT irc initState
  return ()
