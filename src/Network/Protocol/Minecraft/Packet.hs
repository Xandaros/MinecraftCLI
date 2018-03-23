{-# LANGUAGE DeriveGeneric, DeriveFunctor, RecordWildCards, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
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

[packets|
[Clientbound]
EncryptionRequest LoggingIn 1
    serverID :: NetworkText
    pubKey :: LengthBS
    verifyToken :: LengthBS
    deriving (Generic, Show)
    instance (Binary)

LoginSuccess LoggingIn 2
    uuid :: NetworkText
    username :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

SetCompression LoggingIn 3
    threshold :: VarInt
    deriving (Show, Generic)
    instance (Binary)

ChatMessage Playing 0x0F
    chatMessage :: NetworkText
    position :: Word8
    deriving (Show, Generic)
    instance (Binary)

ConfirmTransaction Playing 0x11
    windowId :: Int8
    actionNumber :: Int16
    accepted :: Bool
    deriving (Show, Generic)
    instance (Binary)

WindowItems Playing 0x14
    windowId :: Int8
    count :: Int16
    deriving (Show, Generic)
    instance (Binary)

SetSlot Playing 0x16
    windowId :: Int8
    slot :: Int16
    slotData :: Slot
    deriving (Show, Generic)
    instance (Binary)

DisconnectPlay Playing 0x1A
    reason :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

JoinGame Playing 0x23
    playerEid :: Int32
    gamemode :: Word8
    dimension :: Dimension
    difficulty :: Word8
    maxPlayers :: Word8
    levelType :: NetworkText
    reducedDebugInfo :: Bool
    deriving (Show, Generic)
    instance (Binary)

KeepAlive Playing 0x1F
    keepAliveId :: Int64
    deriving (Show, Generic)
    instance (Binary)

PlayerPositionAndLook Playing 0x2F
    x :: NetworkDouble
    y :: NetworkDouble
    z :: NetworkDouble
    yaw :: NetworkFloat
    pitch :: NetworkFloat
    flags :: Word8
    posLookID :: VarInt
    deriving (Show, Generic)
    instance (Binary)




[Serverbound]
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

ChatMessage Playing 0x02
    chatMessage :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

ClientStatus Playing 0x03
    actionID :: VarInt
    deriving (Show, Generic)
    instance (Binary)

ClientSettings Playing 0x04
    locale :: NetworkText
    viewDistance :: Int8
    chatMode :: VarInt
    colors :: Bool
    displayedSkinParts :: Word8
    mainHand :: VarInt
    deriving (Show, Generic)
    instance (Binary)

ClickWindow Playing 0x07
    windowId :: Word8
    slot :: Int16
    button :: Int8
    actionNumber :: Int16
    mode :: VarInt
    clickedItem :: Slot
    deriving (Show, Generic)
    instance (Binary)

KeepAlive Playing 0x0B
    keepAliveId :: Int64
    deriving (Show, Generic)
    instance (Binary)

PlayerPositionAndLook Playing 0x0E
    x :: NetworkDouble
    y :: NetworkDouble
    z :: NetworkDouble
    yaw :: NetworkFloat
    pitch :: NetworkFloat
    onGround :: Bool
    deriving (Show, Generic)
    instance (Binary)
|]

instance Binary CBUnknownPayload where
    get = CBUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (CBUnknownPayload a) = putByteString a

instance Binary SBUnknownPayload where
    get = SBUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (SBUnknownPayload a) = putByteString a
