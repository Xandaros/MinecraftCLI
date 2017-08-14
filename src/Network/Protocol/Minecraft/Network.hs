{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}
module Network.Protocol.Minecraft.Network where

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString (ByteString(..))
import           Data.Int
import           Data.Monoid
import           Data.Word
import           GHC.IO.Handle
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           System.IO (IOMode(..))

import Debug.Trace

data ConnectionState = Handshaking
                     | LoggingIn
                     | Playing
                     | GettingStatus

data Packet = PacketHandshake { protocolVersion :: VarInt
                              , address :: String
                              , port :: Word16
                              , nextState :: ConnectionState
                              }
            | PacketUnknown ByteString

newtype VarInt = VarInt {unVarInt :: Int32}
    deriving (Show, Bits, Eq, Ord, Num)
newtype VarLong = VarLong {unVarLong :: Int64}
    deriving (Show, Bits, Eq, Ord, Num)

getPacketID :: Packet -> VarInt
getPacketID PacketHandshake{} = 0x00

packVarInt :: VarInt -> Builder
packVarInt = mconcat . fmap BSB.word8 . packVarVal 5 . unVarInt

packVarLong :: VarLong -> Builder
packVarLong = mconcat . fmap BSB.word8 . packVarVal 10 . unVarLong

packVarVal :: (Show a, Bits a, Integral a) => Int -> a -> [Word8]
packVarVal _ 0 = [0]
packVarVal maxSegs i = go i maxSegs
    where go :: (Show a, Bits a, Integral a) => a -> Int -> [Word8]
          go _ 0 = []
          go 0 _ = []
          go i maxSegs = if newVal == 0
                            then [temp]
                            else temp `setBit` 7 : go newVal (maxSegs - 1)
              where temp = fromIntegral i .&. 0b01111111 :: Word8
                    newVal = i `shiftR` 7

unpackVarInt :: ByteString -> VarInt
unpackVarInt = VarInt . unpackVarVal

unpackVarLong :: ByteString -> VarLong
unpackVarLong = VarLong . unpackVarVal

unpackVarVal :: (Integral a, Bits a, Num a) => ByteString -> a
unpackVarVal bs = go $ BS.unpack bs
    where go :: (Num a, Bits a) => [Word8] -> a
          go [] = 0
          go (x:xs) = if x `testBit` 7
                         then go xs `shiftL` 7 .|. (fromIntegral x .&. 0b01111111)
                         else fromIntegral x .&. 0b01111111

packConnectionState :: ConnectionState -> Builder
packConnectionState Handshaking   = packVarInt 0
packConnectionState GettingStatus = packVarInt 1
packConnectionState LoggingIn     = packVarInt 2
packConnectionState Playing       = packVarInt 3

packString :: String -> Builder
packString = BSB.stringUtf8

packPayload :: Packet -> Builder
packPayload PacketHandshake{..} = packVarInt protocolVersion
                               <> packString address
                               <> BSB.word16BE port
                               <> packConnectionState nextState

packPacket :: Packet -> Builder
packPacket packet = (packVarInt $ packetIDLength + payloadLength) <> packetID <> payload
    where payload = packPayload packet
          payloadLength = fromIntegral $ builderLength payload
          packetID = packVarInt $ getPacketID packet
          packetIDLength = fromIntegral $ builderLength packetID

builderLength :: Builder -> Int64
builderLength = BSL.length . BSB.toLazyByteString

test :: IO ()
test = do
    sock <- socket AF_INET Stream defaultProtocol
    --connect sock (SockAddrInet 25565 (tupleToHostAddress (7, 4, 219, 104)))
    connect sock (SockAddrInet 25565 (tupleToHostAddress (104, 219, 4, 7)))

    putStrLn "Connected"

    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle (BlockBuffering Nothing)

    putStrLn "Got handle"

    let handshake = PacketHandshake 335 "102.219.4.7" 25565 LoggingIn
    BSB.hPutBuilder handle $ packPacket handshake

    putStrLn "Handshake sent"

    test <- BS.hGetContents handle

    putStrLn "Response received:"
    print $ BS.unpack test

    hClose handle
    pure ()
