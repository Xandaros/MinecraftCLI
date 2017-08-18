{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving, ScopedTypeVariables, RecordWildCards #-}
{-# LANGUAGE DefaultSignatures, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Minecraft.Network where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import           GHC.IO.Handle
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           System.IO (IOMode(..))

import Network.Protocol.Minecraft.Network.Encoding
import Network.Protocol.Minecraft.Network.Packet
import Network.Protocol.Minecraft.Network.Parser
import Network.Protocol.Minecraft.Network.Yggdrasil

test :: IO ()
test = do
    sock <- socket AF_INET Stream defaultProtocol
    --connect sock (SockAddrInet 25565 (tupleToHostAddress (7, 4, 219, 104)))
    connect sock (SockAddrInet 25565 (tupleToHostAddress (104, 219, 4, 7)))

    putStrLn "Connected"

    handle <- socketToHandle sock ReadWriteMode
    --hSetBuffering handle (BlockBuffering Nothing)

    putStrLn "Got handle"

    void $ runEncodedT (defaultEncodingState handle) $ do
        let handshake = PacketHandshakePayload 335 "102.219.4.7" 25565 LoggingIn
        sendPacket handshake

        liftIO $ putStrLn "Handshake sent"

        let loginStart = PacketLoginStartPayload "Yotanido"
        sendPacket loginStart

        liftIO $ putStrLn "LoginStart sent"

        response <- readPacket

        liftIO $ putStrLn "Response received"
        let packet = parsePacket LoggingIn response

        liftIO $ putStrLn "decoded pubkey"

        let (Right (PacketEncryptionRequest encRequest)) = packet

        liftIO $ putStrLn "Generating shared secret"

        sharedSecret <- liftIO $ generateSharedKey

        let serverHash = createServerHash (serverID encRequest) sharedSecret (pubKey encRequest)
            joinRequest = JoinRequest "Your auth token" "Your UUID" (Text.pack serverHash)

        joinSucc <- liftIO $ join joinRequest
        liftIO $ putStrLn $ if joinSucc then "Join successful" else "Join failed"

        Just response <- liftIO $ encryptionResponse sharedSecret encRequest
        sendPacket response

        True <- enableEncryption sharedSecret
        pure ()

        Right packet <- parsePacket LoggingIn <$> readPacket
        liftIO $ print packet

        case packet of
          PacketSetCompression (PacketSetCompressionPayload thresh) -> setCompressionThreshold (fromIntegral thresh)
          _ -> pure ()

        packet <- readPacket
        liftIO . print $ BS.unpack packet
        liftIO . print $ packet

    hClose handle
    pure ()
