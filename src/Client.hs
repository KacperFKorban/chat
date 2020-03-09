{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Client
    ( client
    )
where

import           Network.Socket          hiding ( send
                                                , recv
                                                , sendTo
                                                )
import           Network.Socket.ByteString
import           Control.Concurrent
import           Control.Monad.Reader
import           Debug.Trace
import qualified Data.ByteString               as B
import           Data.Text.Encoding
import qualified Data.Text                     as T
import           Network.Multicast
import           Control.Lens

data Env = Env {
    _sockTCP :: Socket,
    _sockUDP :: Socket,
    _connMOut :: (Socket, SockAddr),
    _nick :: T.Text
}

makeLenses ''Env

type App = ReaderT Env IO

client :: IO ()
client = do
    addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
    (sockTCP, sockUDP) <- sockets addr
    sockMIn            <- multicastReceiver "239.0.0.1" 4243
    connMOut           <- multicastSender "239.0.0.1" 4243
    putStrLn "Enter your name: "
    name <- getLine
    void $ forkIO $ handleRequests sockTCP name
    void $ forkIO $ handleRequests sockUDP name
    void $ forkIO $ handleRequests sockMIn name
    let nick = encodeUtf8 $ T.pack name
    void $ forkIO $ void $ send sockTCP nick
    void $ forkIO $ sendAll sockUDP nick
    runReaderT clientHandler $ Env sockTCP sockUDP connMOut $ T.pack name

sockets :: AddrInfo -> IO (Socket, Socket)
sockets addr = do
    sockTCP <- socket (addrFamily addr) Stream defaultProtocol
    sockUDP <- socket (addrFamily addr) Datagram defaultProtocol
    connect sockTCP $ addrAddress addr
    connect sockUDP $ addrAddress addr
    return (sockTCP, sockUDP)

handleRequests :: Socket -> String -> IO ()
handleRequests sock name = do
    line <- recv sock 1024
    let msg = decodeUtf8 line
    if (T.pack name) `T.isPrefixOf` msg
        then handleRequests sock name
        else do
            putStrLn $ T.unpack msg
            handleRequests sock name

clientHandler :: App ()
clientHandler = do
    line <- lift getLine
    let msg = encodeUtf8 $ T.pack line
    case msg of
        "close" -> do
            sock <- asks (^. sockTCP)
            lift . void $ send sock msg
        "U"     -> do
            sock <- asks (^. sockUDP)
            lift . void $ sendAll sock ":)"
            clientHandler
        "M" -> do
            (sock, sockAddr) <- asks (^. connMOut)
            name <- asks (^. nick)
            lift . void $ sendTo sock (encodeUtf8 (name <> ": " <> ":D")) sockAddr
            clientHandler
        _ -> do
            sock <- asks (^. sockTCP)
            lift . void $ send sock msg
            clientHandler
