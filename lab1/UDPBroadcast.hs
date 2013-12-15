{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSLazy8

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Data.Binary
import Data.Time
import System.Time
import System.Posix.User
import Network.BSD
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString

import Message
import MsgInfo

currentUnixTime64 = do
    TOD t _ <- getClockTime
    return $ fromIntegral t

client :: IO ()
client = do 
    addr <- head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just "1234")
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    setSocketOption sock Broadcast 1
    forever $ do
		msg <- encode <$> createMsg -- <$> == fmap
--		print $ "length of created msg " ++ show (BSL.length msg)
		x <- sendAllTo sock (BSL.toStrict msg) $ addrAddress addr
--		print $ "Msg sent " ++ (show x)
		threadDelay 1000000
 where
  createMsg :: IO Message
  createMsg = do
    name <- getHostName 
    ip <- hostFromSockAddr <$> addrAddress <$> head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE], addrFamily = AF_INET}))
        (Just name) (Just "1234")
    host <- BS8.pack <$> getLoginName
    time <- currentUnixTime64
    return $ Message ip host time (BS8.pack name)
   where 
     hostFromSockAddr (SockAddrInet _ a) = a


server :: MVar MsgInfo -> IO ()
server info = do
    addr <- head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just "1234")
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    putStrLn $ "Starting server loop"
    forever $ do 
--		putStrLn ".. Server receiving"
		(t, _) <- recvFrom sock 10000
		let msg = decode $ (BSL.fromStrict t) :: Message 
		insertMsg msg info
		--modifyMVar_ info (insertMsg msg)


printer :: MVar MsgInfo -> IO ()
printer info = forever $ do
	log <- showMsgInfo <$> readMVar info
	let len = maximum $ 1:map length (lines log)
	putStrLn $ replicate len '='
	putStrLn log
	putStrLn $ replicate len '='
	threadDelay 1000000


main = do
	info <- newMVar $ emptyInfo
	sequence_ $ replicate 10 (forkIO client)
	let foreverServer = (server info) `catch` (\(e :: SomeException) -> foreverServer)
	forkIO $ foreverServer
	printer info
	
	
