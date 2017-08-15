{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Minecraft.Network where

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Monoid
import           Data.Text (Text)
import           Data.Word
import           GHC.Generics
import           GHC.IO.Handle
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           System.IO (IOMode(..))

import Network.Protocol.Minecraft.Network.Types

data ConnectionState = Handshaking
                     | LoggingIn
                     | Playing
                     | GettingStatus
                     deriving (Show)

class HasPacketID f where
    getPacketID :: f -> VarInt
    mode :: f -> ConnectionState

data PacketHandshake = PacketHandshake { protocolVersion :: VarInt
                                       , address :: Text
                                       , port :: Word16
                                       , nextState :: ConnectionState
                                       } deriving (Generic, Show)
instance Packable PacketHandshake

instance HasPacketID PacketHandshake where
    getPacketID _ = 0x00
    mode _ = Handshaking

data PacketLoginStart = PacketLoginStart { username :: Text
                                         } deriving (Generic, Show)
instance Packable PacketLoginStart

instance HasPacketID PacketLoginStart where
    getPacketID _ = 0x00
    mode _ = LoggingIn

data PacketUnknown = PacketUnknown ByteString

instance Packable ConnectionState where
    pack = packConnectionState

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
packConnectionState Handshaking   = pack (0 :: VarInt)
packConnectionState GettingStatus = pack (1 :: VarInt)
packConnectionState LoggingIn     = pack (2 :: VarInt)
packConnectionState Playing       = pack (3 :: VarInt)

packPacket :: (Packable a, HasPacketID a) => a -> Builder
packPacket packet = (pack $ (packetIDLength + payloadLength :: VarInt)) <> packetID <> payload
    where payload = pack packet
          payloadLength = fromIntegral $ builderLength payload
          packetID = pack $ (getPacketID packet :: VarInt)
          packetIDLength = fromIntegral $ builderLength packetID

builderLength :: Builder -> Int64
builderLength = BSL.length . BSB.toLazyByteString

hGetPacketLength :: Handle -> IO VarInt
hGetPacketLength = fmap (unpackVarInt . BS.pack . reverse) . go []
    where go :: [Word8] -> Handle -> IO [Word8]
          go rest handle = do
              fstByte <- flip BS.index 0 <$> BS.hGet handle 1
              if fstByte `testBit` 7
                 then go (fstByte : rest) handle
                 else pure $ fstByte : rest

test :: IO ()
test = do
    sock <- socket AF_INET Stream defaultProtocol
    --connect sock (SockAddrInet 25565 (tupleToHostAddress (7, 4, 219, 104)))
    connect sock (SockAddrInet 25565 (tupleToHostAddress (104, 219, 4, 7)))

    putStrLn "Connected"

    handle <- socketToHandle sock ReadWriteMode
    --hSetBuffering handle (BlockBuffering Nothing)

    putStrLn "Got handle"

    let handshake = PacketHandshake 335 "102.219.4.7" 25565 LoggingIn
    BSB.hPutBuilder handle $ packPacket handshake

    putStrLn "Handshake sent"

    let loginStart = PacketLoginStart "Xandaros"
    BSB.hPutBuilder handle $ packPacket loginStart

    putStrLn "LoginStart sent"

    len <- hGetPacketLength handle
    response <- BS.hGet handle (fromIntegral len)

    putStrLn "Response received:"
    print len
    print $ BS.unpack response

    hClose handle
    pure ()
