{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Server
    ( chat
    )
where

import           Network.Socket          hiding ( send
                                                , recv
                                                , recvFrom
                                                , sendTo
                                                )
import           Network.Socket.ByteString
import           Control.Concurrent
import           Control.Lens
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           System.IO
import           Debug.Trace
import           Control.Concurrent.Lifted
import qualified Data.ByteString               as B
import           Data.Foldable
import           Data.Text.Encoding
import qualified Data.Text                     as T
import           Control.Error.Util
import           Data.List
import           Data.Maybe

data ConnectionTCP = ConnectionTCP {
    _sock :: Socket,
    _nickTCP :: T.Text
} deriving (Eq, Show)

makeLenses ''ConnectionTCP

data ConnectionUDP = ConnectionUDP {
    _sockAddr :: SockAddr,
    _nickUDP :: T.Text
} deriving (Eq, Show)

makeLenses ''ConnectionUDP

data Env = Env {
    _channels :: TMVar [ConnectionTCP],
    _sockUDP :: Socket,
    _sockTCP :: Socket
}

makeLenses ''Env

type App = ReaderT Env IO

chat :: IO ()
chat = do
    sockTCP <- startSocket Stream
    sockUDP <- startSocket Datagram
    getSocketName sockTCP >>= \name -> putStrLn $ "Started TCP server on address: " <> show name
    getSocketName sockUDP >>= \name -> putStrLn $ "Started UDP server on address: " <> show name
    listen sockTCP 2
    tmvTCP <- newTMVarIO []
    forkIO $ runReaderT (runUDP []) $ Env tmvTCP sockUDP sockTCP
    runReaderT mainLoop $ Env tmvTCP sockUDP sockTCP

startSocket :: SocketType -> IO Socket
startSocket socketType = do
    addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
    sock <- socket (addrFamily addr) socketType defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    return sock

mainLoop :: App ()
mainLoop = do
    sock <- asks (^. sockTCP)
    conn <- lift $ accept sock
    liftIO $ putStrLn $ "Accepted new connection: " <> show conn
    fork $ startConn $ conn ^. _1
    mainLoop

runUDP :: [ConnectionUDP] -> App ()
runUDP addrss = do
    sock      <- asks (^. sockUDP)
    (m, from) <- lift $ recvFrom sock 1024
    let msg = T.strip $ decodeUtf8 m
    lift $ putStrLn $ "UDP got: " <> T.unpack msg <> " from " <> show from
    let addrs = find ((== from) . (^. sockAddr)) addrss
    if isJust addrs
        then do
            sendMsgUDP (fromJust addrs ^. nickUDP <> ": " <> msg)
                $   filter (/= from)
                $   (^. sockAddr)
                <$> addrss
            runUDP addrss
        else runUDP $ ConnectionUDP from msg : addrss

addChannel :: ConnectionTCP -> TMVar [ConnectionTCP] -> STM ()
addChannel conn channels =
    takeTMVar channels >>= \chs -> putTMVar channels $ conn : chs

removeChannel :: ConnectionTCP -> TMVar [ConnectionTCP] -> STM ()
removeChannel conn channels =
    takeTMVar channels >>= \chs -> putTMVar channels $ filter (/= conn) chs

startConn :: Socket -> App ()
startConn sock = do
    chs <- asks (^. channels)
    m   <- lift $ recv sock 1024
    let name       = T.strip $ decodeUtf8 m
        conn = ConnectionTCP sock name
    lift . atomically $ addChannel conn chs
    void . fork $ runConn conn

runConn :: ConnectionTCP -> App ()
runConn conn = do
    let s = conn ^. sock
    m <- lift $ recv s 1024
    let msg = T.strip $ decodeUtf8 m
    lift $ putStrLn $ "TCP got: " <> T.unpack msg
    chs <- asks (^. channels)
    if msg == "close"
        then do
            lift . forkIO . atomically $ removeChannel conn chs
            lift $ close s
        else do
            fork . sendMsgToOthers conn $ encodeUtf8 msg
            runConn conn

sendMsgToOthers :: ConnectionTCP -> B.ByteString -> App ()
sendMsgToOthers conn msg = do
    chs       <- asks (^. channels)
    actualChs <- lift . atomically . readTMVar $ chs
    let formattedMsg = (encodeUtf8 $ conn ^. nickTCP) <> ": " <> msg
    lift
        . traverse_ (\ch -> forkIO $ void $ send (ch ^. sock) formattedMsg)
        $ filter (/= conn) actualChs

sendMsgUDP :: T.Text -> [SockAddr] -> App ()
sendMsgUDP msg addrss = do
    chs       <- asks (^. channels)
    actualChs <- lift . atomically . readTMVar $ chs
    sock      <- asks (^. sockUDP)
    lift $ traverse_
        (forkIO . void . sendTo sock (encodeUtf8 msg))
        addrss
