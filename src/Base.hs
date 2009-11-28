{-# LANGUAGE Rank2Types #-}

module Base (
    me
  , dotDir
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
  , BotConfig (..)
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
import Control.Monad (forever, forM_, foldM, liftM, liftM2)
import Control.Monad.State.Strict (StateT, runStateT, get, put)
import Data.Map (Map)
import qualified Data.Map as M
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         getAppUserDataDirectory)
import System.Exit (exitWith, ExitCode(..))
import System.IO (Handle, hGetLine, hPutStr, hSetBuffering, BufferMode (..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import qualified Network.IRC.Base as IRC
import qualified Network.IRC.Parser as IRC
import Network (PortID(..), PortNumber, connectTo)

import Util((+++), io, doForever, mcoin, dropNewlines, maybeIO, ellipsesSplit)


type Nick = String
type NetName = String
type SrvName = String
type ChannelName = String
type RootMap = Map NetName [Nick]

data Srv = Srv SrvName PortNumber
data Channel = Channel NetName ChannelName
             deriving (Eq, Ord, Read, Show)
-- has enough data to allow connections. multiple servers allow
-- redundant connections
data Net = Net NetName [Srv]

data Event = Join Channel Nick
           | Part Channel Nick String
           | ChannelMsg Channel Nick String
           | PrivMsg NetName Nick String -- nick is sending user
           | Ping NetName String
             deriving (Show)

data Action = DoJoin Channel
            | DoPart Channel String
            | DoChannelMsg Channel String
            | DoPrivMsg NetName Nick String
            | DoInit NetName Nick
            | DoPong NetName String

-- Father forgive me
dotDir :: FilePath
dotDir = unsafePerformIO $ getAppUserDataDirectory "eigenbot"

ircMaxLineLen :: Int
ircMaxLineLen = 512

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
type PluginLoop s = (Read s) => EvQ -> ActQ -> PluginStateT s
type Plugin = EvQ -> ActQ -> IO ()

-- messy. FilePath is location of saved state, if any
genPlugin :: (Read s) => String -> PluginLoop s -> s -> Maybe FilePath -> Plugin
genPlugin description loop initState savedFile =
    \evq actq -> do
       printf "plugin starting: %s\n" description
       state <- case savedFile of
                  Nothing -> return initState
                  Just f -> do
                    exists <- doesFileExist f
                    if exists
                      then read `liftM` readFile f
                      else return initState
       runStateT (forever $ loop evq actq) state
       mcoin

runPlugin :: Plugin -> EvQ -> ActQ -> IO ()
runPlugin plug evq actq = plug evq actq

data BotConfig = BotConfig {
      bcNets :: [Net] -- networks to always connect to
    , bcChannels :: [Channel] -- should make sure Channel's net is connected to
    , bcRoots :: RootMap
    , bcPlugins :: [Plugin]
}

-- FIXME network connection requests are not implemented.
-- do I really want request lists? probably. how about channel part
-- lists? how to track dialog between actions and events, so errors
-- can be reported back?
data IrcState = IS {
      -- nets and chans I'm in
      stInNets :: [Net]
    , stInChannels :: [Channel]
      -- nets and chans I'd like to be in
    , stReqNets :: [Net]
    , stReqChannels :: [Channel]
    , stRoots :: RootMap -- per-network "root" users
    , stPlugins :: [Plugin] -- not sure how to dynamically load plugins
    , stEvq :: EvQ
    , stActq :: ActQ
}

type Irc a = StateT IrcState IO a



-- FIXME better: map of networks to nicks kept in state
me :: Nick
me = "eigenbot"

isMe :: Nick -> Bool
isMe = (== me)

say :: ActQ -> Channel -> String -> IO ()
say actq chan msg = addAction actq $ DoChannelMsg chan msg

pm :: ActQ -> NetName -> Nick -> String -> IO ()
pm actq net nick msg = addAction actq $ DoPrivMsg net nick msg

pong :: ActQ -> NetName -> String -> IO ()
pong actq net msg = addAction actq $ DoPong net msg

part :: ActQ -> Channel -> String -> IO ()
part actq chan msg = addAction actq $ DoPart chan msg


type IrcCmd = String

genMsg, genMsgColon :: IrcCmd -> String -> String
genMsg cmd rest = printf "%s %s\r\n" cmd rest
genMsgColon cmd rest = genMsg cmd (':' : rest)

-- Find the base length of a colon command. Useful for deciding how to
-- split long messages.

-- FIXME this doesn't take our nick+user+hostname part into account,
-- and so gives a smaller value than is actually correct! eigenbot
-- needs to handle more IRC events (in particular, the welcome message
-- from the server) and store its hostname to be able to do this
-- correctly!
genMsgColonLen :: IrcCmd -> Int
genMsgColonLen cmd = length $ genMsgColon cmd ""

{- may be useful some day.
netOfEvent :: Event -> NetName
netOfEvent (Join (Channel n _) _nick) = n
netOfEvent (Part (Channel n _) _nick _msg) = n
netOfEvent (ChannelMsg (Channel n _) _nick _msg) = n
netOfEvent (PrivMsg n _nick _msg) = n
netOfEvent (Ping n _msg) = n
-}

netOfAction :: Action -> NetName
netOfAction (DoJoin (Channel n _)) = n
netOfAction (DoPart (Channel n _) _msg) = n
netOfAction (DoChannelMsg (Channel n _) _msg) = n
netOfAction (DoPrivMsg n _nick _msg) = n
netOfAction (DoInit n _nick) = n
netOfAction (DoPong n _msg) = n

-- use irc's Message?

-- invariants for DoChannelMsg: the base length of the IRC command
-- (given by genMsgColonLen) must be less than ircMaxLineLen; the
-- allowedLen must be big enough to allow messages returned from
-- ellipsesSplit to fit (otherwise, you'll get a flippant message,
-- though no crashes)
actionToMsgs :: Action -> [String]
actionToMsgs (DoJoin (Channel _ chan)) = [genMsg "JOIN" chan]
actionToMsgs (DoPart (Channel _ chan) msg) = [genMsgColon ("PART"+++chan) msg]
actionToMsgs (DoInit _net nick) = [genMsg "NICK" nick ++ genMsg "USER" (nick ++ " 0 * :realname")]
actionToMsgs (DoPong _net str) = [genMsgColon "PONG" str]
actionToMsgs (DoPrivMsg _net nick msg) = [genMsgColon ("PRIVMSG"+++nick) msg]
actionToMsgs (DoChannelMsg (Channel _ chan) msg) = map (genMsgColon cmd) msgs
  where cmd = "PRIVMSG"+++chan
        msgs = case ellipsesSplit msg allowedLen of
                 Just ms -> ms
                 Nothing -> ["jli: aren't you glad this is a Maybe? :)"]
        allowedLen = ircMaxLineLen - (genMsgColonLen cmd)

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
        then Just $ PrivMsg net nick rest
        else Just $ ChannelMsg (Channel net toPart) nick rest
      _ -> Nothing
parseEvent _net (IRC.Message (Just (IRC.Server _name)) _ _) = Nothing
parseEvent net (IRC.Message Nothing cmd params) =
    case cmd of
      "PING" -> Just $ Ping net (params !! 0)
      _ -> Nothing




-- should, like, clean up and wait on threads and stuff, somewhere
runBot :: BotConfig -> IO ()
runBot config = do
    initState <- configToInitState config
    runStateT setup initState
    mcoin

configToInitState :: BotConfig -> IO IrcState
configToInitState (BotConfig nets chans roots plugins) =
    liftM2 (\evq actq -> IS { stInNets = [], stInChannels = []
                            , stReqNets = nets, stReqChannels = chans
                            , stRoots = roots, stPlugins = plugins
                            , stEvq = evq, stActq = actq })
           newEvq newActq

-- needs some hslogger love
-- need to move comments from here to some fancy architecture overview
setup :: Irc ()
setup = do
    io $ putStrLn "begin setup"
    io $ setupDirs
    -- called with state derived from BotConfig, so inNets and inChans
    -- will be empty
    st@(IS [] [] reqNets reqChans _roots plugins evq actq) <- get
    -- hook up incoming events to evq through network parser, start
    -- actq handler for dispatching actions to networks
    io $ putStrLn "connect nets"
    inNets <- io $ connectNets reqNets evq actq
    -- all plugins get all events
    io $ startPlugins plugins evq actq
    inChans <- io $ joinChans reqChans actq
    put $ st { stInNets = inNets, stInChannels = inChans
             , stReqNets = [], stReqChannels = [] }
    io $ putStrLn "enter listen loop"
    forever $ mainLoop
  where setupDirs = createDirectoryIfMissing True dotDir

-- main loop that controls IRC state. ultimately, would like to be
-- able to control all aspects of behavior (network connections,
-- channel membership, plugin activation). currently just handles
-- requests to join, part, and die
mainLoop :: Irc ()
mainLoop = do
    handleNewRequests
    handleEvent

handleNewRequests :: Irc ()
handleNewRequests = do
    st@(IS inNets inChans reqNets reqChans _roots _plugins _evq actq) <- get
    (reqNets', newNets) <- io $ connectNewNets actq reqNets
    (reqChans', newChans) <- io $ joinNewChans actq reqChans
    put $ st { stInNets = inNets ++ newNets
             , stInChannels = inChans ++ newChans
             , stReqNets = reqNets'
             , stReqChannels = reqChans' }
  where connectNewNets _ [] = return ([], [])
        connectNewNets _actq _requests = do
          putStrLn "NETWORK CONNECTION REQUEST FAILED. Not implemented yet."
          return ([], [])
        joinNewChans actq requests = do
          joinChans requests actq
          return ([], requests)

handleEvent :: Irc ()
handleEvent = do
    IS _inNets _inChans _reqNets _reqChans roots _plugins evq actq <- get
    ev <- io $ readEvent evq
    case ev of
      Ping net msg -> io $ pong actq net msg
      Join _chan _nick -> mcoin
      Part _chan _nick _msg -> mcoin
      ChannelMsg _chan _nick _msg -> mcoin -- leave to the plugins
      PrivMsg net nick msg ->
        if isRoot roots net nick
        then controller net nick msg
        else io $ putStrLn "privmsg but NOT FROM A ROOT!"

controller :: NetName -> Nick -> String -> Irc ()
controller net nick msg = do
    st@(IS _inNets _inChans _reqNets reqChans _roots _plugins _evq actq) <- get
    let reply = pm actq net nick
    case words msg of
      ["!join", chan] -> do
        io $ reply "adding to requested channels"
        put $ st { stReqChannels = (Channel net chan) : reqChans }
      ["!part", chan] ->
        io $ do reply "leaving now..."
                part actq (Channel net chan) "PUPPETS SAY YES"
      "!say":chan:rest ->
        io $ do reply "saying..."
                say actq (Channel net chan) $ unwords rest
      "!pm":dstNick:rest ->
        io $ do reply "pming..."
                pm actq net dstNick $ unwords rest
      ["!die"] ->
        -- HANDLE STATE SERIALIZATION
        io $ do reply "dying!!!"
                exitWith ExitSuccess
      _ ->
        io $ reply "didn't understand that! !join, !part, !say, !pm, !die"


isRoot :: RootMap -> NetName -> Nick -> Bool
isRoot roots net nick =
    case M.lookup net roots of
      Nothing -> False
      Just nicks -> nick `elem` nicks

-- currently assumes all attempts to connect to networks will be
-- successful (like joinChans).
connectNets :: [Net] -> EvQ -> ActQ -> IO [Net]
connectNets nets evq actq = do
    handleMap <- buildHandleMap nets
    forkNetHandlers handleMap
    forkActionHandler handleMap
    initConnects handleMap
    return $ nets

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
  let net = netOfAction act
  case M.lookup net handleMap of
    Nothing -> error $ printf "no handle for net %s\n" net
    Just h -> do
      mapM_ (printf "to %s <%s>\n" net) (map dropNewlines $ actionToMsgs act)
      sendActionToNet h act


sendActionToNet :: Handle -> Action -> IO ()
sendActionToNet h act = mapM_ (hPutStr h) (actionToMsgs act)

startPlugins :: [Plugin] -> EvQ -> ActQ -> IO ()
startPlugins plugs evq actq = forM_ plugs startPlug
    where startPlug p = do
            -- duplicate event queue so all plugins get all events
            evq' <- dupEvq evq
            forkIO $ runPlugin p evq' actq

-- currently just assumes we will successfully join any channel we try
-- (like connectNets). perhaps better would be to return [] and only
-- add channels to the internal state's inChannels list when we get a
-- response from the IRC server.
joinChans :: [Channel] -> ActQ -> IO [Channel]
joinChans chans actq = do
    forM_ chans (\c -> addAction actq $ DoJoin c)
    return $ chans
