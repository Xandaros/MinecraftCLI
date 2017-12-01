{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals, FlexibleInstances, OverloadedStrings, DeriveLift, LambdaCase, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Protocol.Minecraft.Types where

import           Control.Applicative (empty, (<|>))
import           Control.Lens (makeFields, (^.))
import           Control.Lens.Iso (Iso', iso)
import           Control.Monad.Identity
import           Data.Aeson
import           Data.Bits
import           Data.Binary (Binary(..))
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as HML
import           Data.Int
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import           Data.Word
import           GHC.Generics
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

class NetworkVar src dst | src -> dst, dst -> src where
    network :: Iso' src dst

newtype NetworkText = NetworkText {unNetworkText :: Text}
    deriving (Show, Eq, Ord, IsString, Monoid)

instance Binary NetworkText where
    get = do
        len <- fromIntegral <$> (get :: Get VarInt)
        NetworkText . TE.decodeUtf8 <$> getByteString len

    put (NetworkText text) = do
        let bs = TE.encodeUtf8 text
        put (fromIntegral $ (BS.length bs) :: VarInt)
        putByteString bs

instance NetworkVar Text NetworkText where
    network = iso NetworkText unNetworkText

newtype NetworkFloat = NetworkFloat {unNetworkFloat :: Float}
    deriving (Show, Eq, Ord, Num, Floating, Fractional, Enum, Real, RealFloat, RealFrac)

instance Binary NetworkFloat where
    get = NetworkFloat <$> getFloatbe
    put = putFloatbe . unNetworkFloat

instance NetworkVar Float NetworkFloat where
    network = iso NetworkFloat unNetworkFloat

newtype NetworkDouble = NetworkDouble {unNetworkDouble :: Double}
    deriving (Show, Eq, Ord, Num, Floating, Fractional, Enum, Real, RealFloat, RealFrac)

instance Binary NetworkDouble where
    get = NetworkDouble <$> getDoublebe
    put = putDoublebe . unNetworkDouble

instance NetworkVar Double NetworkDouble where
    network = iso NetworkDouble unNetworkDouble

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

data ClickEvent = ClickEvent deriving (Generic, Show)
data HoverEvent = HoverEvent deriving (Generic, Show)

instance FromJSON ClickEvent
instance FromJSON HoverEvent


data Test f = Test (f Bool)

data ChatShared f = ChatShared { chatSharedBold :: f Bool
                               , chatSharedItalic :: f Bool
                               , chatSharedUnderlined :: f Bool
                               , chatSharedStrikethrough :: f Bool
                               , chatSharedObfuscated :: f Bool
                               , chatSharedColor :: f Text
                               , chatSharedInsertion :: Maybe Text
                               , chatSharedClickEvent :: ClickEvent
                               , chatSharedHoverEvent :: HoverEvent
                               } deriving (Generic)

defaultChatShared :: ChatShared Maybe
defaultChatShared = ChatShared Nothing Nothing Nothing Nothing Nothing Nothing Nothing ClickEvent HoverEvent

baseChatShared :: ChatShared Identity
baseChatShared = ChatShared false false false false false (Identity "white") Nothing ClickEvent HoverEvent
    where false = Identity False

deriving instance (Show (f Bool), Show (f Text)) => Show (ChatShared f)

instance FromJSON (ChatShared Maybe) where
    parseJSON (Object o) = ChatShared <$> o .:? "bold"
                                      <*> o .:? "italic"
                                      <*> o .:? "underlined"
                                      <*> o .:? "strikethrough"
                                      <*> o .:? "obfuscated"
                                      <*> o .:? "color"
                                      <*> o .:? "insertion"
                                      <*> pure ClickEvent
                                      <*> pure HoverEvent
    parseJSON _ = empty

data ChatComponent f = StringComponent { chatComponentShared :: ChatShared f
                                       , chatComponentText :: Text
                                       , chatComponentExtra :: [ChatComponent f]
                                       }
                     | TranslationComponent { chatComponentShared :: ChatShared f
                                            , chatComponentTranslate :: Text
                                            , chatComponentWith :: [ChatComponent f]
                                            , chatComponentExtra :: [ChatComponent f]
                                            }
makeFields ''ChatShared
makeFields ''ChatComponent

deriving instance (Show (f Bool), Show (f Text)) => Show (ChatComponent f)

instance FromJSON (ChatComponent Maybe) where
    parseJSON (Object o) | "text" `HML.member` o = StringComponent <$> parseJSON (Object o)
                                                                   <*> o .: "text"
                                                                   <*> (o .: "extra" <|> pure [])
                         | "translate" `HML.member` o = TranslationComponent <$> parseJSON (Object o)
                                                                             <*> o .: "translate"
                                                                             <*> o .: "with"
                                                                             <*> (o .: "extra" <|> pure [])
                         | otherwise = empty
    parseJSON (String s) = pure $ StringComponent defaultChatShared s []
    parseJSON (Array a) | not (V.null a) = do
        first <- parseJSON (V.head a)
        rest <- sequence . V.toList $ parseJSON <$> V.drop 1 a
        pure $ StringComponent defaultChatShared first rest
    parseJSON _ = empty

inheritStyle :: ChatShared Identity -> ChatShared Maybe -> ChatShared Identity
inheritStyle base cc = ChatShared (Identity $ fromMaybe (runIdentity $ base ^. bold) (cc ^. bold))
                                  (Identity $ fromMaybe (runIdentity $ base ^. italic) (cc ^. italic))
                                  (Identity $ fromMaybe (runIdentity $ base ^. underlined) (cc ^. underlined))
                                  (Identity $ fromMaybe (runIdentity $ base ^. strikethrough) (cc ^. strikethrough))
                                  (Identity $ fromMaybe (runIdentity $ base ^. obfuscated) (cc ^. obfuscated))
                                  (Identity $ fromMaybe (runIdentity $ base ^. color) (cc ^. color))
                                  (cc ^. insertion)
                                  ClickEvent
                                  HoverEvent

inheritChatComponent :: ChatShared Identity -> ChatComponent Maybe -> ChatComponent Identity
inheritChatComponent base cc = case cc of
                                 StringComponent{} -> StringComponent canonicalStyle (cc ^. text) extras
                                 TranslationComponent{} -> let withs = inheritChatComponent canonicalStyle <$> cc ^. with
                                                           in  TranslationComponent canonicalStyle (cc ^. translate) withs extras
    where canonicalStyle = inheritStyle base (cc ^. shared)
          extras = inheritChatComponent canonicalStyle <$> cc ^. extra

canonicalizeChatComponent :: ChatComponent Maybe -> ChatComponent Identity
canonicalizeChatComponent = inheritChatComponent baseChatShared

chatToText :: ChatComponent Identity -> Text
chatToText (TranslationComponent _ key withs extra) = case key of
                                                        "chat.type.text" -> "<" <> chatToText (withs !! 0) <> "> "
                                                                         <> chatToText (withs !! 1) <> extras
                                                        "commands.message.display.incoming" -> chatToText (withs !! 0) <> " whispers to you: "
                                                                                            <> chatToText (withs !! 1) <> extras
                                                        _ -> key <> extras
    where extras = mconcat (chatToText <$> extra)
chatToText (StringComponent _ t extra) = t <> mconcat (chatToText <$> extra)

data Slot = Slot { slotBlockId :: Int16
                 , slotItemCount :: Maybe Int8
                 , slotItemDamage :: Maybe Int8
                 -- NBT
                 } deriving (Show)
makeFields ''Slot

instance Binary Slot where
    get = do
        blockId <- getInt16be
        if blockId == -1
           then pure $ Slot (-1) Nothing Nothing
           else Slot blockId <$> (Just <$> getInt8) <*> (Just <$> getInt8)
    put (Slot id count dmg) = do
        putInt16be id
        case count of
          Just c -> putInt8 c
          Nothing -> pure ()
        case dmg of
          Just d -> putInt8 d
          Nothing -> pure ()

emptySlot :: Slot
emptySlot = Slot (-1) Nothing Nothing

isEmptySlot :: Slot -> Bool
isEmptySlot (Slot (-1) _ _) = True
isEmptySlot _ = False

-- TODO:
-- Entity Metadata
-- NBT Tag
-- Position
-- Angle (Word8)
-- UUID
-- 
