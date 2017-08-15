{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Minecraft.Network where

import           Crypto.PubKey.RSA
import           Crypto.PubKey.RSA.PKCS15
import           Crypto.Random (getRandomBytes)
import           Data.ASN1.BinaryEncoding (DER(..))
import           Data.ASN1.Encoding
import           Data.ASN1.Types(fromASN1)
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
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

decodePubKey :: ByteString -> Maybe PublicKey
decodePubKey keyBytes = do
    Right asn1 <- pure $ decodeASN1' DER keyBytes
    Right (PubKeyRSA key, _) <- pure $ fromASN1 asn1
    pure key

generateSharedKey :: IO ByteString
generateSharedKey = getRandomBytes 16

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

    putStrLn "Response received"
    let packet = parsePacket LoggingIn response

    putStrLn "decoded pubkey"

    let (Right (PacketEncryptionRequest encRequest)) = packet

    let Just publicKey = decodePubKey (pubKey encRequest)

    putStrLn "Generating shared secret"

    sharedSecret <- generateSharedKey

    Right encryptedSecret <- encrypt publicKey sharedSecret
    Right encryptedVerifyToken <- encrypt publicKey $ verifyToken encRequest

    let response = PacketEncryptionResponsePayload { secretLen = 128
                                                   , secret = encryptedSecret
                                                   , responseVerifyTokenLen = 128
                                                   , responseVerifyToken = encryptedVerifyToken
                                                   }
    BSB.hPutBuilder handle $ packPacket response

    len <- hGetPacketLength handle
    response <- BS.hGet handle (fromIntegral len)
    print $ BS.unpack response

    hClose handle
    pure ()
