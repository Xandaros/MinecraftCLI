{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Foreign.Crypt ( newCipher
                     , closeCipher
                     , decrypt
                     , Cipher
                     ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Foreign
import Foreign.C.String
import Foreign.C.Types

data CCipher
type Cipher = Ptr CCipher

foreign import ccall "crypt.h newCipher" c_newCipher :: CString -> CString -> IO Cipher

foreign import ccall "crypt.h closeCipher" c_closeCipher :: Cipher -> IO ()

foreign import ccall "crypt.h decrypt" c_decrypt :: Cipher -> CString -> CInt -> IO ()


newCipher :: ByteString -> ByteString -> IO Cipher
newCipher key iv = BS.useAsCStringLen key $ \(ckey, _) ->
    BS.useAsCStringLen iv $ \(civ, _) ->
        c_newCipher ckey civ

closeCipher :: Cipher -> IO ()
closeCipher = c_closeCipher

decrypt :: Cipher -> ByteString -> IO ByteString
decrypt c cipher = BS.useAsCStringLen cipher $ \(ccipher, len) -> do
    c_decrypt c ccipher (fromIntegral len)
    BS.packCStringLen (ccipher, len)
