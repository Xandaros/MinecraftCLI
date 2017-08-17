{-# LANGUAGE RecordWildCards #-}
module Network.Protocol.Minecraft.Network.Encoding where --( generateSharedKey
                                                   --, encryptionResponse
                                                   --, decrypt
                                                   --, getCipher
                                                   --, cfb8Decrypt
                                                   --, cfb8Encrypt
                                                   --) where

import           Control.Monad
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
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           Data.X509
import           Network.Protocol.Minecraft.Network.Packet
import           Numeric


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

decrypt :: AES128 -> IV AES128 -> ByteString -> ByteString
decrypt secret iv dat = cfbDecrypt secret iv dat

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
