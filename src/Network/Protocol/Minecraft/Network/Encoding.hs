{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Network.Protocol.Minecraft.Network.Encoding where --( generateSharedKey
                                                   --, encryptionResponse
                                                   --, decrypt
                                                   --, getCipher
                                                   --, cfb8Decrypt
                                                   --, cfb8Encrypt
                                                   --) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Crypto.Cipher.AES (AES128)
import           Crypto.Cipher.Types
import           Crypto.Hash
import           Crypto.Error (CryptoFailable(..))
import           Crypto.PubKey.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
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
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           Data.Word
import           Data.X509
import           GHC.IO.Handle
import           Network.Protocol.Minecraft.Network.Packet
import           Network.Protocol.Minecraft.Network.Types
import           Numeric

data EncryptionState = EncryptionState { aes :: AES128
                                       , encryptSR :: ByteString
                                       , decryptSR :: ByteString
                                       }

data EncodingState = EncodingState { encryptionState :: Maybe EncryptionState
                                   , compressionThreshold :: Int
                                   , handle :: Handle
                                   }

newtype EncodedT m a = EncodedT { unEncodedT :: StateT EncodingState m a }
    deriving (Functor, Applicative, Monad, MonadState EncodingState, MonadTrans, MonadIO)

runEncodedT :: EncodingState -> EncodedT m a -> m (a, EncodingState)
runEncodedT initialState = flip runStateT initialState . unEncodedT

decodePubKey :: ByteString -> Maybe PublicKey
decodePubKey keyBytes = do
    Right asn1 <- pure $ decodeASN1' DER keyBytes
    Right (PubKeyRSA key, _) <- pure $ fromASN1 asn1
    pure key

generateSharedKey :: IO ByteString
generateSharedKey = getRandomBytes 16

getCipher :: ByteString -> Maybe AES128
getCipher secret = do
    CryptoPassed cipher <- pure $ cipherInit secret
    pure cipher

defaultEncodingState :: Handle -> EncodingState
defaultEncodingState handle = EncodingState Nothing (-1) handle

enableEncryption :: Monad m => ByteString -> EncodedT m Bool
enableEncryption secret = do
    case getCipher secret of
        Nothing -> pure False
        Just cipher -> do
            modify (\s -> s{encryptionState = Just $ EncryptionState cipher secret secret})
            pure True

setCompressionThreshold :: Monad m => Int -> EncodedT m ()
setCompressionThreshold threshold = modify (\s -> s{compressionThreshold = threshold})

decrypt :: Monad m => ByteString -> EncodedT m ByteString
decrypt ciphertext = do
    encState' <- gets encryptionState
    case encState' of
      Nothing -> pure ciphertext
      Just encState -> do
          let sr = decryptSR encState
              cipher = aes encState
              (ret, newSR) = cfb8Decrypt cipher sr ciphertext
              newEncState = encState{decryptSR = newSR}
          modify $ \s -> s{encryptionState = Just newEncState}
          pure ret

encrypt :: Monad m => ByteString -> EncodedT m ByteString
encrypt plaintext = do
    encState' <- gets encryptionState
    case encState' of
      Nothing -> pure plaintext
      Just encState -> do
          let sr = encryptSR encState
              cipher = aes encState
              (ret, newSR) = cfb8Encrypt cipher sr plaintext
              newEncState = encState{encryptSR = newSR}
          modify $ \s -> s{encryptionState = Just newEncState}
          pure ret

readPacket :: MonadIO m => EncodedT m ByteString
readPacket = do
    handle <- gets handle
    len <- readVarInt
    compThresh <- gets compressionThreshold
    decompressedLength <- if compThresh >= 0
                             then readVarInt
                             else pure 0
    payload <- liftIO (BS.hGet handle (fromIntegral len)) >>= decrypt
    if decompressedLength > 0
       then error "Compression not implemented"
       else pure payload

sendPacket :: (MonadIO m, Packable p, HasPacketID p) => p -> EncodedT m ()
sendPacket packet = do
    handle <- gets handle
    let packedPacket = packPacket packet
    liftIO $ BSB.hPutBuilder handle packedPacket

readVarInt :: MonadIO m => EncodedT m VarInt
readVarInt = fmap (unpackVarInt . BS.pack . reverse) $ go []
    where go :: MonadIO m => [Word8] -> EncodedT m [Word8]
          go rest = do
              handle <- gets handle
              fstByteBS <- liftIO $ BS.hGet handle 1
              decryptedFstByteBS <- decrypt fstByteBS
              let fstByte = decryptedFstByteBS `BS.index` 0
              if fstByte `testBit` 7
                 then go (fstByte : rest)
                 else pure $ fstByte : rest

encryptionResponse :: ByteString -> PacketEncryptionRequestPayload -> IO (Maybe PacketEncryptionResponsePayload)
encryptionResponse secret PacketEncryptionRequestPayload{..} = runMaybeT $ do
    publicKey <- maybeZero $ decodePubKey pubKey
    encryptedSecret <- eitherAToMaybeT $ RSA.encrypt publicKey secret
    encryptedToken <- eitherAToMaybeT $ RSA.encrypt publicKey verifyToken
    pure $ PacketEncryptionResponsePayload { secretLen = 128
                                           , secret = encryptedSecret
                                           , responseVerifyTokenLen = 128
                                           , responseVerifyToken = encryptedToken
                                           }

maybeZero :: (MonadPlus m) => Maybe a -> m a
maybeZero = maybe mzero pure

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

eitherAToMaybeT :: Applicative m => m (Either a b) -> MaybeT m b
eitherAToMaybeT = MaybeT . fmap eitherToMaybe

-- Function taken from https://github.com/Lazersmoke/civskell/blob/ebf4d761362ee42935faeeac0fe447abe96db0b5/src/Civskell/Tech/Encrypt.hs#L154-L165
-- Encrypt a bytestring using the cfb8 aes128 cipher, and the provided shift register
cfb8Encrypt :: AES128 -> BS.ByteString -> BS.ByteString -> (BS.ByteString,BS.ByteString)
cfb8Encrypt c i = BS.foldl magic (BS.empty,i)
  where
    -- Does a single step (one byte) of a CFB8 encryption
    -- add the cipher text to the output, and return the updated shift register
    magic (ds,iv) d = (ds `BS.snoc` ct,ivFinal)
      where
        -- use the MSB of the encrypted shift register to encrypt the current plaintext
        ct = BS.head (ecbEncrypt c iv) `xor` d
        -- shift the new ciphertext into the shift register
        ivFinal = BS.tail iv `BS.snoc` ct

-- Function taken from https://github.com/Lazersmoke/civskell/blob/ebf4d761362ee42935faeeac0fe447abe96db0b5/src/Civskell/Tech/Encrypt.hs#L167-L175
-- Decrypt a bytestring using the cfb8 aes128 cipher, and the provided shift register
cfb8Decrypt :: AES128 -> BS.ByteString -> BS.ByteString -> (BS.ByteString,BS.ByteString)
cfb8Decrypt c i = BS.foldl magic (BS.empty,i)
  where
    magic (ds,iv) d = (ds `BS.snoc` pt,ivFinal)
      where
        pt = BS.head (ecbEncrypt c iv) `xor` d
        -- snoc on cipher always
        ivFinal = BS.tail iv `BS.snoc` d

createServerHash :: Text -> ByteString -> ByteString -> String
createServerHash serverId' secret pubKey =
    let serverId = TE.encodeUtf8 serverId'
        digest :: Digest SHA1
        digest = hashFinalize $ hashUpdates hashInit [serverId, secret, pubKey]
        protoHash :: Integer
        protoHash = fst . head . readHex . show $ digest  -- FIXME
        isNegative = protoHash `testBit` 159
    in  if isNegative
           then let hash = (2^160 - 1) `xor` (protoHash - 1)
                in  "-" ++ showHex hash ""
           else showHex protoHash ""

packPacket :: (Packable a, HasPacketID a) => a -> Builder
packPacket packet = (pack $ (packetIDLength + payloadLength :: VarInt)) <> packetID <> payload
    where payload = pack packet
          payloadLength = fromIntegral $ builderLength payload
          packetID = pack $ (getPacketID packet :: VarInt)
          packetIDLength = fromIntegral $ builderLength packetID

          builderLength :: Builder -> Int64
          builderLength = BSL.length . BSB.toLazyByteString

