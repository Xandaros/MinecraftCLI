{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveGeneric #-}
module Network.Protocol.Minecraft.Encoding ( generateSharedKey
                                           , encryptionResponse
                                           , setCompressionThreshold
                                           , enableEncryption
                                           , createServerHash
                                           , defaultEncodingState
                                           , runEncodedT
                                           , EncodedT
                                           , sendPacket
                                           , readPacket
                                           ) where

import qualified Codec.Compression.Zlib as Zlib
import           Control.Lens ((^.))
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
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           Data.Word
import           Data.X509
import qualified Foreign.Crypt as Crypt
import           GHC.IO.Handle
import           GHC.Generics
import           Network.Protocol.Minecraft.Packet
import           Network.Protocol.Minecraft.Types
import           Numeric

data EncryptionState = EncryptionState { aes :: AES128
                                       , encryptSR :: ByteString
                                       , decryptSR :: Crypt.Cipher
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

enableEncryption :: MonadIO m => ByteString -> EncodedT m Bool
enableEncryption secret = do
    case getCipher secret of
        Nothing -> pure False
        Just cipher -> do
            decryptor <- liftIO $ Crypt.newCipher secret secret
            modify (\s -> s{encryptionState = Just $ EncryptionState cipher secret decryptor})
            pure True

setCompressionThreshold :: Monad m => Int -> EncodedT m ()
setCompressionThreshold threshold = modify (\s -> s{compressionThreshold = threshold})

decrypt :: MonadIO m => ByteString -> EncodedT m ByteString
decrypt ciphertext = do
    encState' <- gets encryptionState
    case encState' of
      Nothing -> pure ciphertext
      Just encState -> do
          let sr = decryptSR encState
          ret <- liftIO $ Crypt.decrypt sr ciphertext
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

data CompressedPacket = CompressedPacket VarInt ByteString
    deriving (Generic)

instance Binary CompressedPacket where
    get = CompressedPacket <$> Binary.get <*> (BSL.toStrict <$> Binary.getRemainingLazyByteString)

    put (CompressedPacket compLen payload) = Binary.put compLen >> Binary.putByteString payload

readPacket :: MonadIO m => ConnectionState -> EncodedT m (Maybe CBPacket)
readPacket state = do
    closed <- connectionClosed
    if closed
       then pure Nothing
       else Just . Binary.runGet (getPacket state) . BSL.fromStrict <$> readPacketData

readPacketData :: MonadIO m => EncodedT m ByteString
readPacketData = do
    handle <- gets handle
    len <- readVarInt
    compThresh <- gets compressionThreshold
    packet <- liftIO (BS.hGet handle (fromIntegral len)) >>= decrypt
    let (decompressedLength, payload) = if compThresh >= 0
                                            then let (CompressedPacket decompressedLength payload) = Binary.decode (BSL.fromStrict packet)
                                                 in  (decompressedLength, payload)
                                            else (0, packet)
    if decompressedLength > 0
       then pure . BSL.toStrict . Zlib.decompress . BSL.fromStrict $ payload
       else pure payload

connectionClosed :: MonadIO m => EncodedT m Bool
connectionClosed = gets handle >>= liftIO . hIsEOF

sendPacket :: (MonadIO m, Binary p, HasPacketID p) => p -> EncodedT m ()
sendPacket packet = do
    handle <- gets handle
    compThresh <- gets compressionThreshold
    let payload = Binary.runPut $ Binary.put packet
        packetID = Binary.runPut . Binary.put $ getPacketID packet
        compressedData = Zlib.compress $ packetID <> payload
        payloadLength = fromIntegral $ BSL.length payload
        dataLength = packetIDLength + payloadLength
        dataLength' = Binary.runPut $ Binary.put dataLength
        dataLength'length = fromIntegral $ BSL.length dataLength'
        compressedLength = fromIntegral $ BSL.length compressedData
        packetIDLength = fromIntegral $ BSL.length packetID
        packedPacket = Binary.runPut $
            if compThresh < 0
               then do
                   Binary.put (dataLength :: VarInt)
                   Binary.putLazyByteString packetID
                   Binary.putLazyByteString payload
               else if fromIntegral dataLength < compThresh
                   then do
                       Binary.put (dataLength + 1 :: VarInt)
                       Binary.put (0 :: VarInt)
                       Binary.putLazyByteString packetID
                       Binary.putLazyByteString payload
                   else do
                       Binary.put (compressedLength + dataLength'length :: VarInt)
                       Binary.put dataLength
                       Binary.putLazyByteString compressedData
    encrypt (BSL.toStrict packedPacket) >>= liftIO . BS.hPut handle

readVarInt :: MonadIO m => EncodedT m VarInt
readVarInt = fmap (snd . unpackVarInt . BS.pack . reverse) $ go []
    where go :: MonadIO m => [Word8] -> EncodedT m [Word8]
          go rest = do
              handle <- gets handle
              fstByteBS <- liftIO $ BS.hGet handle 1
              decryptedFstByteBS <- decrypt fstByteBS
              let fstByte = decryptedFstByteBS `BS.index` 0
              if fstByte `testBit` 7
                 then go (fstByte : rest)
                 else pure $ fstByte : rest

encryptionResponse :: ByteString -> CBEncryptionRequestPayload -> IO (Maybe SBEncryptionResponsePayload)
encryptionResponse secret encryptionRequest = runMaybeT $ do
    publicKey <- maybeZero $ decodePubKey (lengthBS $ encryptionRequest ^. pubKey)
    encryptedSecret <- eitherAToMaybeT $ RSA.encrypt publicKey secret
    encryptedToken <- eitherAToMaybeT $ RSA.encrypt publicKey (lengthBS $ encryptionRequest ^. verifyToken)
    pure $ SBEncryptionResponsePayload { sBEncryptionResponsePayloadSharedSecret = LengthBS 128 encryptedSecret
                                       , sBEncryptionResponsePayloadVerifyToken = LengthBS 128 encryptedToken
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

createServerHash :: Text -> ByteString -> ByteString -> String
createServerHash serverId' secret pubKey =
    let serverId = TE.encodeUtf8 serverId'
        digest :: Digest SHA1
        digest = hashFinalize $ hashUpdates hashInit [serverId, secret, pubKey]
        protoHash :: Integer
        protoHash = fst . head . readHex . show $ digest  -- FIXME
        isNegative = protoHash `testBit` 159
    in  if isNegative
           then let hash = (2^(160 :: Int) - 1) `xor` (protoHash - 1)
                in  "-" ++ showHex hash ""
           else showHex protoHash ""
