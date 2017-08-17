{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Minecraft.Network where

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import qualified Data.Text as Text
import           Data.Monoid
import           Data.Word
import           GHC.IO.Handle
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           System.IO (IOMode(..))

import Network.Protocol.Minecraft.Network.Encoding
import Network.Protocol.Minecraft.Network.Packet
import Network.Protocol.Minecraft.Network.Parser
import Network.Protocol.Minecraft.Network.Types
import Network.Protocol.Minecraft.Network.Yggdrasil

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

    let loginStart = PacketLoginStartPayload "Yotanido"
    BSB.hPutBuilder handle $ packPacket loginStart

    putStrLn "LoginStart sent"

    len <- hGetPacketLength handle
    response <- BS.hGet handle (fromIntegral len)

    putStrLn "Response received"
    let packet = parsePacket LoggingIn response

    putStrLn "decoded pubkey"

    let (Right (PacketEncryptionRequest encRequest)) = packet

    putStrLn "Generating shared secret"

    sharedSecret <- generateSharedKey

    let serverHash = createServerHash (serverID encRequest) sharedSecret (pubKey encRequest)
        joinRequest = JoinRequest "Your auth token" "Your UUID" (Text.pack serverHash)

    joinSucc <- join joinRequest
    putStrLn $ if joinSucc then "Join successful" else "Join failed"

    Just response <- encryptionResponse sharedSecret encRequest

    BSB.hPutBuilder handle $ packPacket response

    response <- BS.hGetContents handle

    let Just aes = getCipher sharedSecret

    let (decryptedResponse, _) = cfb8Decrypt aes sharedSecret response

    print . BS.unpack $ decryptedResponse
    print decryptedResponse
    print . BS.unpack $ response

    hClose handle
    pure ()
