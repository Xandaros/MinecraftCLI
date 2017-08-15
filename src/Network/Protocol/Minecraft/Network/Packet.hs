{-# LANGUAGE DeriveGeneric #-}
module Network.Protocol.Minecraft.Network.Packet where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Network.Protocol.Minecraft.Network.Types

data Packet = PacketHandshake PacketHandshakePayload
            | PacketLoginStart PacketLoginStartPayload
            | PacketEncryptionRequest PacketEncryptionRequestPayload
            deriving (Show)

data ConnectionState = Handshaking
                     | LoggingIn
                     | Playing
                     | GettingStatus
                     deriving (Show)

class HasPacketID f where
    getPacketID :: f -> VarInt
    mode :: f -> ConnectionState

data PacketHandshakePayload = PacketHandshakePayload { protocolVersion :: VarInt
                                                     , address :: Text
                                                     , port :: Word16
                                                     , nextState :: ConnectionState
                                                     } deriving (Generic, Show)
instance Packable PacketHandshakePayload

instance HasPacketID PacketHandshakePayload where
    getPacketID _ = 0x00
    mode _ = Handshaking

data PacketLoginStartPayload = PacketLoginStartPayload { username :: Text
                                                       } deriving (Generic, Show)
instance Packable PacketLoginStartPayload

instance HasPacketID PacketLoginStartPayload where
    getPacketID _ = 0x00
    mode _ = LoggingIn

data PacketUnknown = PacketUnknown ByteString

instance Packable ConnectionState where
    pack = packConnectionState

packConnectionState :: ConnectionState -> Builder
packConnectionState Handshaking   = pack (0 :: VarInt)
packConnectionState GettingStatus = pack (1 :: VarInt)
packConnectionState LoggingIn     = pack (2 :: VarInt)
packConnectionState Playing       = pack (3 :: VarInt)

data PacketEncryptionRequestPayload = PacketEncryptionRequestPayload { serverID :: Text
                                                                     , pubKeyLen :: VarInt
                                                                     , pubKey :: [Word8]
                                                                     , verifyTokenLen :: VarInt
                                                                     , verifyToken :: [Word8]
                                                                     } deriving (Generic, Show)

instance Packable PacketEncryptionRequestPayload

instance HasPacketID PacketEncryptionRequestPayload where
    getPacketID _ = 0x01
    mode _ = LoggingIn
