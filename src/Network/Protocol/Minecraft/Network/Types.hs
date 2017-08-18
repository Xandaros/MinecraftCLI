{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals, FlexibleInstances #-}
module Network.Protocol.Minecraft.Network.Types where

import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           GHC.Generics

--------------------------------------------------------------------------------------------------------------
-- Packable
--------------------------------------------------------------------------------------------------------------

class Packable f where
    pack :: f -> Builder

    default pack :: (Generic f, GPackable (Rep f)) => f -> Builder
    pack x = gpack (from x)

class GPackable f where
    gpack :: f a -> Builder

instance GPackable U1 where
    gpack U1 = mempty

instance (GPackable a, GPackable b) => GPackable (a :*: b) where
    gpack (a :*: b) = gpack a <> gpack b

instance GPackable a => GPackable (M1 i c a) where
    gpack (M1 x) = gpack x

instance Packable a => GPackable (K1 i a) where
    gpack (K1 x) = pack x

instance Packable Text where
    pack s = pack len <> str
        where len = fromIntegral . BSL.length . BSB.toLazyByteString $ str :: VarInt
              str = TE.encodeUtf8Builder s

instance Packable Word8 where
    pack = BSB.word8

instance Packable Int8 where
    pack = BSB.int8

instance Packable Word16 where
    pack = BSB.word16BE

instance Packable Int16 where
    pack = BSB.int16BE

instance Packable Int32 where
    pack = BSB.int32BE

instance Packable Int64 where
    pack = BSB.int64BE

instance Packable Float where
    pack = BSB.floatBE

instance Packable Double where
    pack = BSB.doubleBE

instance Packable Bool where
    pack False = pack (0 :: Word8)
    pack True = pack (1 :: Word8)

instance Packable a => Packable (Maybe a) where
    pack Nothing = mempty
    pack (Just x) = pack x

instance Packable a => Packable [a] where
    pack [] = mempty
    pack (x:xs) = pack x <> pack xs

instance Packable ByteString where
    pack = pack . BS.unpack

--------------------------------------------------------------------------------------------------------------

newtype VarInt = VarInt {unVarInt :: Int32}
    deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

newtype VarLong = VarLong {unVarLong :: Int64}
    deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

instance Packable VarInt where
    pack = mconcat . fmap BSB.word8 . packVarVal 5 . unVarInt

instance Packable VarLong where
    pack = mconcat . fmap BSB.word8 . packVarVal 10 . unVarLong

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

-- TODO:
-- Chat
-- Entity Metadata
-- Slot
-- NBT Tag
-- Position
-- Angle (Word8)
-- UUID
-- 
