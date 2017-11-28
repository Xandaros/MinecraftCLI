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

[packetsCB|
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
    keepAliveId :: VarInt
    deriving (Show, Generic)
    instance (Binary)

PlayerPositionAndLook Playing 0x2E
    x :: NetworkDouble
    y :: NetworkDouble
    z :: NetworkDouble
    yaw :: NetworkFloat
    pitch :: NetworkFloat
    flags :: Word8
    posLookID :: VarInt
    deriving (Show, Generic)
    instance (Binary)

WindowItems Playing 0x14
    windowItemsWindowId :: Int8
    count :: Int16
    deriving (Show, Generic)
    instance (Binary)

SetSlot Playing 0x16
    setSlotWindowId :: Int8
    slot :: Int16
    slotData :: Slot
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
    chatMessage :: NetworkText
    deriving (Show, Generic)
    instance (Binary)

ClientStatus Playing 0x04
    actionID :: VarInt
    deriving (Show, Generic)
    instance (Binary)

ClientSettings Playing 0x05
    locale :: NetworkText
    viewDistance :: Int8
    chatMode :: VarInt
    colors :: Bool
    displayedSkinParts :: Word8
    mainHand :: VarInt
    deriving (Show, Generic)
    instance (Binary)

ClickWindow Playing 0x08
    windowId :: Word8
    slot :: Int16
    button :: Int8
    actionNumber :: Int16
    mode :: VarInt
    clickedItem :: Slot
    deriving (Show, Generic)
    instance (Binary)

KeepAlive Playing 0x0C
    keepAliveId :: VarInt
    deriving (Show, Generic)
    instance (Binary)

PlayerPositionAndLook Playing 0x0F
    x :: NetworkDouble
    y :: NetworkDouble
    z :: NetworkDouble
    yaw :: NetworkFloat
    pitch :: NetworkFloat
    onGround :: Bool
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
      0x0F -> CBChatMessage <$> get
      0x14 -> CBWindowItems <$> get
      0x16 -> CBSetSlot <$> get
      0x1A -> CBDisconnectPlay <$> get
      0x1F -> CBKeepAlive <$> get
      0x23 -> CBJoinGame <$> get
      0x2E -> CBPlayerPositionAndLook <$> get
      _ -> CBUnknown <$> get
getPacket _ = CBUnknown <$> get

instance Binary CBUnknownPayload where
    get = CBUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (CBUnknownPayload a) = putByteString a

instance Binary SBUnknownPayload where
    get = SBUnknownPayload . BSL.toStrict <$> getRemainingLazyByteString
    put (SBUnknownPayload a) = putByteString a

lensify ''CBPacket
lensify ''SBPacket
