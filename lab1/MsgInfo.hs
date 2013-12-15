module MsgInfo where

import Message
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Control.Concurrent

type MsgInfo = Map IPAddress (Message, Integer, ThreadId)
emptyInfo :: MsgInfo 
emptyInfo = M.empty

insertMsg :: Message -> MVar MsgInfo -> IO ()
insertMsg msg mm = do
	m <- takeMVar mm
	let oldMsg = M.lookup (ip msg) m
	case oldMsg of 
		Nothing -> do
			tid <- forkIO $ do
				threadDelay 15000000
				modifyMVar_ mm (return . (M.delete (ip msg)))
			putMVar mm (M.insert (ip msg) (msg, 1, tid) m)
		Just (oldMsg, n, tid) -> do
			killThread tid
			tid <- forkIO $ do
				threadDelay 15000000
				modifyMVar_ mm (return . (M.delete (ip msg)))
			putMVar mm (M.insert (ip msg) (oldMsg, (n + 1), tid) m)

showMsgInfo :: MsgInfo -> String
showMsgInfo m = intercalate "\n" [ "Received " ++ (show cnt) ++ " messages from " ++ (show msg) | (k, (msg, cnt, tid)) <- M.toList m]
