{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals, FlexibleInstances, OverloadedStrings, DeriveLift, LambdaCase, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
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
import           Language.Haskell.TH.Syntax (Lift)

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

data LengthBS = LengthBS { lengthBSLen :: VarInt
                         , lengthBS :: ByteString
                         } deriving (Show)

instance Binary LengthBS where
    put LengthBS{..} = put lengthBSLen >> putByteString lengthBS
    get = do
        len <- get
        LengthBS len <$> getByteString (fromIntegral len)

newtype VarInt = VarInt {unVarInt :: Int32}
    deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

newtype VarLong = VarLong {unVarLong :: Int64}
    deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

instance Binary VarInt where
    put = putByteString . packVarInt

    get = do
        initial <- getWhile (`testBit` 7)
        last <- getByteString 1
        pure . snd . unpackVarInt $ initial <> last

instance Binary VarLong where
    put = putByteString . packVarLong

    get = do
        initial <- getWhile (`testBit` 7)
        last <- getByteString 1
        pure . snd . unpackVarLong $ initial <> last

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

packVarInt :: VarInt -> ByteString
packVarInt vi = BS.pack $ packVarVal 5 (fromIntegral vi :: Word32)

packVarLong :: VarLong -> ByteString
packVarLong vl = BS.pack $ packVarVal 10 (fromIntegral vl :: Word64)

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

newtype NetworkFloat = NetworkFloat {unNetworkFloat :: Float}
    deriving (Show, Eq, Ord, Num, Floating, Fractional, Enum, Real, RealFloat, RealFrac)

instance Binary NetworkFloat where
    get = NetworkFloat <$> getFloatbe
    put = putFloatbe . unNetworkFloat

newtype NetworkDouble = NetworkDouble {unNetworkDouble :: Double}
    deriving (Show, Eq, Ord, Num, Floating, Fractional, Enum, Real, RealFloat, RealFrac)

instance Binary NetworkDouble where
    get = NetworkDouble <$> getDoublebe
    put = putDoublebe . unNetworkDouble

getWhile :: (Word8 -> Bool) -> Get ByteString
getWhile p = fmap (fromMaybe "") . lookAheadM $ do
    byte <- getWord8
    if p byte
       then (Just . (BS.pack [byte] <>)) <$> getWhile p
       else pure Nothing

data ConnectionState = Handshaking
                     | LoggingIn
                     | Playing
                     | GettingStatus
                     deriving (Show, Lift)

class HasPacketID f where
    getPacketID :: f -> VarInt
    mode :: f -> ConnectionState

class HasPayload f a | f -> a where
    getPayload :: f -> a

instance Binary ConnectionState where
    put Handshaking   = put (0 :: VarInt)
    put GettingStatus = put (1 :: VarInt)
    put LoggingIn     = put (2 :: VarInt)
    put Playing       = put (3 :: VarInt)

    get = getWord8 >>= pure . \case
                   0 -> Handshaking
                   1 -> GettingStatus
                   2 -> LoggingIn
                   3 -> Playing
                   _ -> error "Unknown state"


-- TODO:
-- Chat
-- Entity Metadata
-- Slot
-- NBT Tag
-- Position
-- Angle (Word8)
-- UUID
-- 
