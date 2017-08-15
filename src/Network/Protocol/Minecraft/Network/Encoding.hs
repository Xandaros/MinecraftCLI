{-# LANGUAGE RecordWildCards #-}
module Network.Protocol.Minecraft.Network.Encoding ( generateSharedKey
                                                   , encryptionResponse
                                                   , decrypt
                                                   , getCipher
                                                   ) where

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Crypto.Cipher.AES (AES128)
import           Crypto.Cipher.Types
import           Crypto.Error (CryptoFailable(..))
import           Crypto.PubKey.RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import           Crypto.Random (getRandomBytes)
import           Data.ASN1.BinaryEncoding (DER(..))
import           Data.ASN1.Encoding
import           Data.ASN1.Types(fromASN1)
import           Data.ByteString (ByteString)
import           Data.X509
import           Network.Protocol.Minecraft.Network.Packet


decodePubKey :: ByteString -> Maybe PublicKey
decodePubKey keyBytes = do
    Right asn1 <- pure $ decodeASN1' DER keyBytes
    Right (PubKeyRSA key, _) <- pure $ fromASN1 asn1
    pure key

generateSharedKey :: IO ByteString
generateSharedKey = getRandomBytes 16

getCipher :: ByteString -> Maybe (AES128, IV AES128)
getCipher secret = do
    iv <- makeIV secret
    CryptoPassed cipher <- pure $ cipherInit secret
    pure (cipher, iv)

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
