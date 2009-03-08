{-# OPTIONS
    -XMultiParamTypeClasses
    -XFunctionalDependencies
#-}

module Network.TCPServer(
    TCPServer(..),
    AcceptData,
    Connections,
    TCPManager(..)) where

import System.IO
import Data.Map

import Control.Process
import Network

-- Manager protocol
data TCPManager msg
-- External
        = Stop
        | Kill
        | GetConnections (Process (Connections msg))
-- Internal
        | TcpServer (Process (), Socket)
        | Register (Process msg) AcceptData
        | Unregister (Process msg)
            deriving Show


data Register = Allow | Deny

type AcceptData = (Handle, HostName, PortNumber)
type Connections msg = Map (Process msg) AcceptData

class TCPServer st msg | msg -> st, st -> msg where
    -- User Definition
    tcpHandler :: st -> AcceptData -> Proc msg ()

    -- Interface
    startManager :: st -> Bool -> Integer -> Proc t (Process (TCPManager msg))
    startManager st osRun port = do
        manager <- spawnOS $ tcpManager Allow Nothing empty
        socket <- liftIO $ listenOn $ PortNumber $ fromIntegral port

        let tcpServer :: Proc () ()
            tcpServer = do
                acceptData <- liftIO $ accept socket
                msg <- recvMaybe
                case msg of
                    Nothing -> do
                        (if osRun then spawnOS else spawn)
                            (connection acceptData st manager)
                        tcpServer
                    Just () -> return ()

        server <- spawnOS tcpServer
        send manager $ TcpServer (server, socket)
        return manager

    stopManager, killManager :: Process (TCPManager msg) -> Proc t ()
    stopManager manager = send manager Stop >> waitProcess_ manager
    killManager manager = send manager Kill >> waitProcess_ manager

    getConnections :: Process (TCPManager msg) -> Proc t (Connections msg)
    getConnections manager = valProc $ do
        send manager . GetConnections =<< self
        recv

stopTCPServer :: Socket -> Process () -> Proc t ()
stopTCPServer socket server = do
    send server ()
    port <- liftIO $ socketPort socket
    h <- liftIO $ connectTo "localhost" port
    liftIO $ hClose h
    waitProcess_ server

-- System
connection :: TCPServer st msg => AcceptData -> st -> Process (TCPManager msg) -> Proc msg ()
connection acceptData@(handle, _, _) st manager = do
    me <- self
    send manager $ Register me acceptData
    finallyProc (tcpHandler st acceptData) $ do
        send manager $ Unregister me
        tryProc $ liftIO $ hClose handle
        return ()

-- Connection manager
tcpManager :: TCPServer st msg =>
    Register -> Maybe (Process (), Socket) -> Connections msg
        -> Proc (TCPManager msg) ()

    -- - receive tcpServer process
tcpManager Allow Nothing _ = do
    msg <- recv
    case msg of
        TcpServer j ->
            tcpManager Allow (Just j) empty
        _ -> fail "invalid start message"

    -- - user commands
tcpManager Allow j@(Just (tcpServer, socket)) connections = do
    msg <- recv
    case msg of
        Register connection acceptData ->
            tcpManager Allow j $ insert connection acceptData connections
        Unregister connection ->
            tcpManager Allow j $ delete connection connections
        GetConnections who ->
            send who connections >> tcpManager Allow j connections
        Stop -> do
            --kill tcpServer
            stopTCPServer socket tcpServer
            tcpManager Deny j connections
        Kill -> do
            stopTCPServer socket tcpServer
            (`mapM_` toList connections) $ \(connection, (handle, _, _)) -> do
                kill connection
                tryProc $ liftIO $ hClose handle
                return ()
            liftIO $ sClose socket
        x -> fail ("Invalid manage message" ++ show x)

    -- Stop message
tcpManager Deny j@(Just (_, socket)) connections
    | Data.Map.null connections = liftIO $ sClose socket
    | otherwise = do
        msg <- recv
        case msg of
            Unregister connection ->
                tcpManager Deny j $ delete connection connections
            x -> fail ("Invalid stop message " ++ show x)