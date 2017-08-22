{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals, FlexibleInstances, OverloadedStrings #-}
module Network.Protocol.Minecraft.Types where

import           Data.Bits
import           Data.Binary (Binary(..))
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Int
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Word

data Dimension = Overworld
               | Nether
               | TheEnd
               deriving (Show, Eq, Ord)

instance Enum Dimension where
    fromEnum Overworld = 0
    fromEnum Nether = -1
    fromEnum TheEnd = 1

    toEnum (-1) = Nether
    toEnum 0 = Overworld
    toEnum 1 = TheEnd
    toEnum x = error $ "Unknown dimension " ++ show x

instance Binary Dimension where
    put = (put :: Int32 -> Put) . fromIntegral . fromEnum
    get = (toEnum . fromIntegral) <$> (get :: Get Int32)

newtype VarInt = VarInt {unVarInt :: Int32}
    deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

newtype VarLong = VarLong {unVarLong :: Int64}
    deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

instance Binary VarInt where
    put = putByteString . BS.pack . packVarVal 5

    get = do
        initial <- getWhile (`testBit` 7)
        last <- getByteString 1
        pure . snd . unpackVarInt $ initial <> last

packVarVal :: (Show a, Bits a, Integral a) => Int -> a -> [Word8]
packVarVal _ 0 = [0]
packVarVal maxSegs' i' = go i' maxSegs'
    where go :: (Show a, Bits a, Integral a) => a -> Int -> [Word8]
          go _ 0 = []
          go 0 _ = []
          go i maxSegs = if newVal == 0
                            then [temp]
                            else temp `setBit` 7 : go newVal (maxSegs - 1)
              where temp = fromIntegral i .&. 0b01111111 :: Word8
                    newVal = i `shiftR` 7

unpackVarInt :: ByteString -> (ByteString, VarInt)
unpackVarInt = fmap VarInt . unpackVarVal

unpackVarLong :: ByteString -> (ByteString, VarLong)
unpackVarLong = fmap VarLong . unpackVarVal

unpackVarVal :: (Integral a, Bits a, Num a) => ByteString -> (ByteString, a)
unpackVarVal bs = go $ BS.unpack bs
    where go :: (Num a, Bits a) => [Word8] -> (ByteString, a)
          go [] = ("", 0)
          go (x:xs) = if x `testBit` 7
                         then let (rest, bit) = go xs
                              in  (rest, bit `shiftL` 7 .|. (fromIntegral x .&. 0b01111111))
                         else (BS.pack xs, fromIntegral x .&. 0b01111111)

newtype NetworkText = NetworkText {unNetworkText :: Text}
    deriving (Show, Eq, Ord, IsString)

instance Binary NetworkText where
    get = do
        len <- fromIntegral <$> (get :: Get VarInt)
        NetworkText . TE.decodeUtf8 <$> getByteString len

    put (NetworkText text) = do
        let bs = TE.encodeUtf8 text
        put (fromIntegral $ (BS.length bs) :: VarInt)
        putByteString bs

getWhile :: (Word8 -> Bool) -> Get ByteString
getWhile p = fmap (fromMaybe "") . lookAheadM $ do
    byte <- getWord8
    if p byte
       then (Just . (BS.pack [byte] <>)) <$> getWhile p
       else pure Nothing

-- TODO:
-- Chat
-- Entity Metadata
-- Slot
-- NBT Tag
-- Position
-- Angle (Word8)
-- UUID
-- 
