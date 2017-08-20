{-# LANGUAGE DeriveGeneric, LambdaCase, DeriveFunctor, RecordWildCards #-}
module Network.Protocol.Minecraft.Network.Packet where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import GHC.Generics
import Network.Protocol.Minecraft.Network.Types

import Debug.Trace

data CBPacket = PacketEncryptionRequest PacketEncryptionRequestPayload
              | PacketSetCompression PacketSetCompressionPayload
              | PacketLoginSuccess PacketLoginSuccessPayload
              | PacketUnknown PacketUnknownPayload
              deriving (Show)

data SBPacket a = SBPacket a
    deriving (Functor)

instance (Binary a, HasPacketID a) => Binary (SBPacket a) where
    put (SBPacket payload) = do
        let putPayload = put payload
            payloadLength = fromIntegral $ putLength putPayload
            putPacketID = put $ (getPacketID payload)
            packetIDLength = fromIntegral $ putLength putPacketID
        put $ (packetIDLength + payloadLength :: VarInt)
        putPacketID
        putPayload
        where putLength :: Put -> Int64
              putLength = BSL.length . runPut
    get = error "Cannot read Serverbound packet"

getPacket :: ConnectionState -> Get CBPacket
getPacket Handshaking = undefined
getPacket LoggingIn = do
    pid <- get :: Get VarInt
    traceShowM pid
    case pid of
      1 -> PacketEncryptionRequest <$> get
      2 -> PacketLoginSuccess <$> get
      3 -> PacketSetCompression <$> get
      _ -> PacketUnknown <$> get
getPacket Playing = PacketUnknown <$> get
getPacket _ = PacketUnknown <$> get

data ConnectionState = Handshaking
                     | LoggingIn
                     | Playing
                     | GettingStatus
                     deriving (Show)

class HasPacketID f where
    getPacketID :: f -> VarInt
    mode :: f -> ConnectionState

data PacketHandshakePayload = PacketHandshakePayload { protocolVersion :: VarInt
                                                     , address :: NetworkText
                                                     , port :: Word16
                                                     , nextState :: ConnectionState
                                                     } deriving (Generic, Show)
instance Binary PacketHandshakePayload

instance HasPacketID PacketHandshakePayload where
    getPacketID _ = 0x00
    mode _ = Handshaking

data PacketLoginStartPayload = PacketLoginStartPayload { username :: NetworkText
                                                       } deriving (Generic, Show)
instance Binary PacketLoginStartPayload

instance HasPacketID PacketLoginStartPayload where
    getPacketID _ = 0x00
    mode _ = LoggingIn

data PacketUnknownPayload = PacketUnknownPayload ByteString
    deriving (Show)

instance Binary PacketUnknownPayload where
    get = PacketUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (PacketUnknownPayload a) = putByteString a

instance Binary ConnectionState where
    put Handshaking   = put (0 :: VarInt)
    put GettingStatus = put (1 :: VarInt)
    put LoggingIn     = put (2 :: VarInt)
    put Playing       = put (3 :: VarInt)

    get = getWord8 >>= pure . \case
                   0 -> Handshaking
                   1 -> GettingStatus
                   2 -> LoggingIn
                   3 -> Playing
                   _ -> error "Unknown state"


data PacketEncryptionRequestPayload = PacketEncryptionRequestPayload { serverID :: NetworkText
                                                                     , pubKeyLen :: VarInt
                                                                     , pubKey :: ByteString
                                                                     , verifyTokenLen :: VarInt
                                                                     , verifyToken :: ByteString
                                                                     } deriving (Generic, Show)

instance Binary PacketEncryptionRequestPayload where
    get = do
        serverId <- get
        pubKeyLen <- get
        pubKey <- getByteString (fromIntegral pubKeyLen)
        verifyTokenLen <- get
        verifyToken <- getByteString (fromIntegral verifyTokenLen)
        pure $ PacketEncryptionRequestPayload serverId pubKeyLen pubKey verifyTokenLen verifyToken
        
instance HasPacketID PacketEncryptionRequestPayload where
    getPacketID _ = 0x01
    mode _ = LoggingIn

data PacketEncryptionResponsePayload = PacketEncryptionResponsePayload { secretLen :: VarInt
                                                                       , secret :: ByteString
                                                                       , responseVerifyTokenLen :: VarInt
                                                                       , responseVerifyToken :: ByteString
                                                                       } deriving (Generic)

instance Binary PacketEncryptionResponsePayload where
    put PacketEncryptionResponsePayload{..} = do
        put secretLen
        putByteString secret
        put responseVerifyTokenLen
        putByteString responseVerifyToken

    get = do
        secretLen <- get
        secret <- getByteString (fromIntegral secretLen)
        responseVerifyTokenLen <- get
        responseVerifyToken <- getByteString (fromIntegral responseVerifyTokenLen)
        pure $ PacketEncryptionResponsePayload secretLen secret responseVerifyTokenLen responseVerifyToken

instance HasPacketID PacketEncryptionResponsePayload where
    getPacketID _ = 0x01
    mode _ = LoggingIn

data PacketSetCompressionPayload = PacketSetCompressionPayload { threshold :: VarInt
                                                               } deriving (Show, Generic)

instance Binary PacketSetCompressionPayload

data PacketLoginSuccessPayload = PacketLoginSuccessPayload { uuid :: NetworkText
                                                           , successUsername :: NetworkText
                                                           } deriving (Show, Generic)

instance HasPacketID PacketLoginSuccessPayload where
    getPacketID _ = 0x02
    mode _ = LoggingIn

instance Binary PacketLoginSuccessPayload
