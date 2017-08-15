{-# LANGUAGE BinaryLiterals #-}
module Network.Protocol.Minecraft.Network.Parser where

import Prelude hiding (take, takeWhile)

import           Data.Attoparsec.ByteString
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           Data.Word
import           Network.Protocol.Minecraft.Network.Packet
import           Network.Protocol.Minecraft.Network.Types

import Debug.Trace

parsePacket :: ConnectionState -> ByteString -> Either String Packet
parsePacket state = parseOnly (packetParser state)

packetParser :: ConnectionState -> Parser Packet
packetParser state = varIntParser >>= payloadParser state

payloadParser :: ConnectionState -> VarInt -> Parser Packet
payloadParser Handshaking _ = mempty <?> "Handshaking has no clientbound packets"
payloadParser LoggingIn 1 = PacketEncryptionRequest <$> encryptionRequestParser
payloadParser LoggingIn x = mempty <?> "Logging in can only handle Packet 1 right now" ++ show x
payloadParser Playing _ = _
payloadParser GettingStatus _ = mempty <?> "NIY"

encryptionRequestParser :: Parser PacketEncryptionRequestPayload
encryptionRequestParser = do
    serverID       <- stringParser
    pubKeyLen      <- varIntParser
    pubKey         <- word8ArrayParser $ fromIntegral pubKeyLen
    verifyTokenLen <- varIntParser
    verifyToken    <- word8ArrayParser $ fromIntegral verifyTokenLen
    pure $ PacketEncryptionRequestPayload serverID pubKeyLen pubKey verifyTokenLen verifyToken

stringParser :: Parser Text
stringParser = do
    strLen <- varIntParser
    TE.decodeUtf8 <$> take (fromIntegral strLen)

varIntParser :: Parser VarInt
varIntParser = do
    initial <- takeWhile (`testBit` 7)
    last <- take 1
    pure . unpackVarInt $ initial <> last

unpackVarInt :: ByteString -> VarInt
unpackVarInt = VarInt . unpackVarVal

unpackVarLong :: ByteString -> VarLong
unpackVarLong = VarLong . unpackVarVal

unpackVarVal :: (Integral a, Bits a, Num a) => ByteString -> a
unpackVarVal bs = go $ BS.unpack bs
    where go :: (Num a, Bits a) => [Word8] -> a
          go [] = 0
          go (x:xs) = if x `testBit` 7
                         then go xs `shiftL` 7 .|. (fromIntegral x .&. 0b01111111)
                         else fromIntegral x .&. 0b01111111

word8ArrayParser :: Int -> Parser [Word8]
word8ArrayParser = fmap BS.unpack . take
