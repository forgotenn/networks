{-# LANGUAGE ScopedTypeVariables #-}
import Network.Socket

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

import Message

bbb :: IO ()
bbb = do
	n <- newMVar 1
	let f c = do
		myN <- takeMVar n
		putMVar n $ myN + 1
		forM_ [1..myN] $ \i -> putChar c
	sequence_ $ map (forkIO . (\x -> f x)) ['a'..'j']

currentUnixTime64 = do
    TOD t _ <- getClockTime
    return $ fromIntegral t

client :: IO ()
client = do 
    addr <- head <$> getAddrInfo Nothing (Just "255.255.255.255") (Just "1234")
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    setSocketOption sock Broadcast 1
    forever $ do
		msg <- BSLazy8.unpack <$> encode <$> createMsg -- <$> == fmap
		sendTo sock msg $ addrAddress addr
		print "Msg sent"
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


server :: MVar Int -> IO ()
server cnt = do
    addr <- head <$> getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
        Nothing (Just "1234")
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    forever $ do 
		(t, len, _) <- recvFrom sock 10000
--		let msg = decode $ BSLazy8.pack t :: Message
--		print msg
		print (t, len, length t)
		modifyMVar_ cnt $ \x -> return (x + 1)

printer :: MVar Int -> IO ()
printer cnt = forever $ do
	toPrint <- readMVar cnt
	print toPrint
	threadDelay 1000000
	
main = do
	cnt <- newMVar 0
	sequence_ $ replicate 10 (forkIO client)
	forkIO $ (server cnt) `catch` (\(e :: SomeException) -> print e)
	printer cnt
	
	
