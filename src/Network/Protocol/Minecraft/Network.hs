{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Minecraft.Network where

import           Data.ASN1.BinaryEncoding (DER(..))
import           Data.ASN1.Encoding
import           Data.ASN1.Types(fromASN1)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import           Data.Monoid
import           Data.Word
import           Data.X509
import           GHC.IO.Handle
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           System.IO (IOMode(..))

import Network.Protocol.Minecraft.Network.Packet
import Network.Protocol.Minecraft.Network.Parser
import Network.Protocol.Minecraft.Network.Types

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

    let handshake = PacketHandshakePayload 335 "102.219.4.7" 25565 LoggingIn
    BSB.hPutBuilder handle $ packPacket handshake

    putStrLn "Handshake sent"

    let loginStart = PacketLoginStartPayload "Xandaros"
    BSB.hPutBuilder handle $ packPacket loginStart

    putStrLn "LoginStart sent"

    len <- hGetPacketLength handle
    response <- BS.hGet handle (fromIntegral len)

    putStrLn "Response received:"
    print len
    print $ BS.unpack response
    let packet = parsePacket LoggingIn response
    print packet

    putStrLn "decoded pubkey:"

    let (Right (PacketEncryptionRequest payload)) = packet

    let (Right asn1) = decodeASN1' DER (BS.pack $ pubKey payload)
        (Right (PubKeyRSA key, _)) = fromASN1 asn1

    print key

    hClose handle
    pure ()
