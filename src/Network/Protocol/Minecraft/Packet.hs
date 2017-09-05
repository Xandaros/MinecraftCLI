{-# LANGUAGE DeriveGeneric, DeriveFunctor, RecordWildCards, QuasiQuotes, DuplicateRecordFields #-}
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
    pubKey :: LengthBS
    verifyToken :: LengthBS
    deriving (Generic, Show)
    instance (Binary)

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

KeepAlive Playing 0x1F
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

[packetsSB|
Handshake Handshaking 0x00
    protocolVersion :: VarInt
    address :: NetworkText
    port :: Word16
    nextState :: ConnectionState
    deriving (Show, Generic)
    instance (Binary)

LoginStart LoggingIn 0x00
    username :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

EncryptionResponse LoggingIn 0x01
    sharedSecret :: LengthBS
    verifyToken :: LengthBS
    deriving (Show, Generic)
    instance (Binary)

TeleportConfirm Playing 0x00
    teleConfirmID :: VarInt
    deriving (Show, Generic)
    instance (Binary)

ChatMessage Playing 0x03
    chatMessageSB :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

ClientSettings Playing 0x05
    clientSettingsLocale :: NetworkText
    clientSettingsViewDistance :: Int8
    clientSettingsChatMode :: VarInt
    clientSettingsColors :: Bool
    clientSettingsDisplayedSkinParts :: Word8
    clientSettingsMainHand :: VarInt
    deriving (Show, Generic)
    instance (Binary)

KeepAlive Playing 0x0C
    keepAliveId :: VarInt
    deriving (Show, Generic)
    instance (Binary)
|]

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

instance Binary CBUnknownPayload where
    get = CBUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (CBUnknownPayload a) = putByteString a
