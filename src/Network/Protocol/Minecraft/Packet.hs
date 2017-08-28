{-# LANGUAGE DeriveGeneric, DeriveFunctor, RecordWildCards, QuasiQuotes #-}
module Network.Protocol.Minecraft.Packet where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import GHC.Generics
import Network.Protocol.Minecraft.Packet.TH
import Network.Protocol.Minecraft.Types

[packetsCB|
EncryptionRequest LoggingIn 1
    serverID :: NetworkText
    pubKeyLen :: VarInt
    pubKey :: ByteString
    verifyTokenLen :: VarInt
    verifyToken :: ByteString
    deriving (Generic, Show)
    instance ()

LoginSuccess LoggingIn 2
    uuid :: NetworkText
    successUsername :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

SetCompression LoggingIn 3
    threshold :: VarInt
    deriving (Show, Generic)
    instance (Binary)

JoinGame Playing 0x23
    playerEid :: Int32
    joinGamemode :: Word8
    joinDimension :: Dimension
    joinDifficulty :: Word8
    maxPlayers :: Word8
    levelType :: NetworkText
    reducedDebugInfo :: Bool
    deriving (Show, Generic)
    instance (Binary)

KeepAlive Playing 0x0C
    keepAliveId :: VarInt
    deriving (Show, Generic)
    instance (Binary)

PlayerPositionAndLook Playing 0x2E
    posLookCBX :: Double
    posLookCBY :: Double
    posLookCBZ :: Double
    posLookCBYaw :: Float
    posLookCBPitch :: Float
    posLookCBFlags :: Word8
    posLookCBID :: VarInt
    deriving (Show, Generic)
    instance (Binary)
|]

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
    case pid of
      1 -> CBEncryptionRequest <$> get
      2 -> CBLoginSuccess <$> get
      3 -> CBSetCompression <$> get
      _ -> CBUnknown <$> get
getPacket Playing = do
    pid <- get :: Get VarInt
    case pid of
      0x1F -> CBKeepAlive <$> get
      0x23 -> CBJoinGame <$> get
      0x2F -> CBPlayerPositionAndLook <$> get
      _ -> CBUnknown <$> get
getPacket _ = CBUnknown <$> get

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

instance Binary CBUnknownPayload where
    get = CBUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (CBUnknownPayload a) = putByteString a


instance Binary CBEncryptionRequestPayload where
    get = do
        serverId <- get
        pubKeyLen <- get
        pubKey <- getByteString (fromIntegral pubKeyLen)
        verifyTokenLen <- get
        verifyToken <- getByteString (fromIntegral verifyTokenLen)
        pure $ CBEncryptionRequestPayload serverId pubKeyLen pubKey verifyTokenLen verifyToken

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


data PacketTeleportConfirmPayload = PacketTeleportConfirmPayload { teleConfirmID :: VarInt
                                                                 } deriving (Show, Generic)
instance Binary PacketTeleportConfirmPayload

instance HasPacketID PacketTeleportConfirmPayload where
    getPacketID _ = 0x00
    mode _ = Playing

data PacketSBChatMessagePayload = PacketSBChatMessagePayload { chatMessageSB :: NetworkText
                                                             } deriving (Show, Generic)
instance Binary PacketSBChatMessagePayload

instance HasPacketID PacketSBChatMessagePayload where
    getPacketID _ = 0x03
    mode _ = Playing

data PacketClientSettingsPayload = PacketClientSettingsPayload { clientSettingsLocale :: NetworkText
                                                               , clientSettingsViewDistance :: Int8
                                                               , clientSettingsChatMode :: VarInt
                                                               , clientSettingsColors :: Bool
                                                               , clientSettingsDisplayedSkinParts :: Word8
                                                               , clientSettingsMainHand :: VarInt
                                                               } deriving (Show, Generic)
instance Binary PacketClientSettingsPayload

instance HasPacketID PacketClientSettingsPayload where
    getPacketID _ = 0x05
    mode _ = Playing
