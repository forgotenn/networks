{-# LANGUAGE ScopedTypeVariables #-}

module Message where

import Data.Word
import Data.ByteString as BS
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as BS8
import Data.Bits
import Control.Applicative
import Data.Time
import System.Time

type IPAddress = Word32
hostnamelength = 20
namelength = 20

data Message = Message {
			 ip :: IPAddress, -- 4
			 host :: ByteString, -- 20
		     time :: Word64, -- 8
		     name:: ByteString -- 20
		   } 

addZeros :: ByteString -> Int -> ByteString
addZeros s k = s `append` BS.replicate (k - BS.length s) 0
	
instance Binary Message where 
	get = do
		ip <- get
		host <- BS.takeWhile (/= 0) <$> getByteString hostnamelength
		time <- get
		name <- BS.takeWhile (/= 0) <$> getByteString namelength
		return $ Message ip host time name
		
	put (Message ip host time name) = do
		putWord32le ip
		putByteString $ addZeros host hostnamelength
		put time
		putByteString $ addZeros name namelength

showIP :: Word32 -> BS.ByteString
showIP a = BS.intercalate (BS.pack [46]) $ 
    Prelude.map (\sh -> BS8.pack $ show $ (a `shiftR` sh) .&. 255) [24, 16, 8, 0]

showTime :: Word64 -> String
showTime time = (show $ TOD (fromIntegral time) 0)

instance Show Message where
	show (Message ip host time name) = "ip=" ++ BS8.unpack (showIP ip) ++ "(hostname=" ++ (BS8.unpack host) ++ ", name=" ++ (BS8.unpack name) ++ ") " ++ (showTime time)
