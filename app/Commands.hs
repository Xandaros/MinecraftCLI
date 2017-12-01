{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf, DataKinds, ScopedTypeVariables #-}
module Commands where

import           Control.Lens
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import           Data.Maybe (fromJust, catMaybes)
import           Data.Monoid ((<>))
import           Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex

import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

import Inventory
import ItemLookup

declareFields [d|
    data ChatMsg = ChatMsg { chatMsgSender :: Text
                           , chatMsgMessage :: Text
                           } deriving (Show)

    data ChatCommand = ChatCommand { chatCommandSender :: Text
                                   , chatCommandCommand :: Text
                                   , chatCommandArguments :: [Text]
                                   } deriving (Show)
    |]

filterCommand :: Reflex t => Text -> Event t ChatCommand -> Event t ChatCommand
filterCommand needle = ffilter ((==needle) . view command)

checkArguments :: Reflex t => [Int] -> Text -> Event t ChatCommand -> Event t Text
checkArguments lens mes = fmapMaybe $ \cmd -> if length (cmd ^. arguments) `notElem` lens
                                                 then Just mes
                                                 else Nothing

helpText :: NonEmpty Text
helpText = "Hi! I'm a bot written by Xandaros and am currently under active development."
        :| [ "Available commands are:"
           , "help, ping, quit, tp, where, inventory, drop"
           -- , "To get more information about a specific command, use \"help <command>\""
           ]

commandMessagesE :: Reflex t => Event t ChatCommand -> Event t (NonEmpty Text)
commandMessagesE cmd =
    let a = mergeList
                [ checkArguments [3,5] "Invalid arguments. tp <x> <y> <z> [<pitch> <yaw>]" (filterCommand "tp" cmd) 
                , checkArguments [1,2] "Invalid arguments. drop <slot> [amount]" (filterCommand "drop" cmd)
                , "Pong!" <$ filterCommand "ping" cmd
                , "Bye :'(" <$ filterCommand "quit" cmd
                ]
        b = helpText <$ filterCommand "help" cmd
    in a <> b

tpCommandE :: Reflex t => Event t ChatCommand -> Event t (Double, Double, Double)
tpCommandE cmd = fforMaybe (filterCommand "tp" cmd) $ \c ->
    let args = c ^. arguments
        getArg n = read $ T.unpack (args !! n)
        x = getArg 0
        y = getArg 1
        z = getArg 2
    in  if | length args == 3 -> Just $ (x, y, z)
           | length args == 5 -> Just $ (x, y, z)
           | otherwise -> Nothing

quitCommandE :: Reflex t => Event t ChatCommand -> Event t ()
quitCommandE = (() <$) . filterCommand "quit"

whereCommandE :: Reflex t => Event t ChatCommand -> Dynamic t (Double, Double, Double) -> Event t SBPacket
whereCommandE cmd pos = chatString . show <$>
    tag (current pos) (filterCommand "where" cmd)

inventoryCommandE :: forall t. Reflex t => Event t ChatCommand -> Dynamic t (Vector 46 Slot) -> Event t SBPacket
inventoryCommandE cmd inventory = chatString . renderSlots <$> inventoryEvents
        where prettySlot :: (Int, Maybe String) -> Maybe String
              prettySlot (_, Nothing) = Nothing
              prettySlot (slot, Just slotData) = Just $ slotData ++ "(" ++ show slot ++ ")"

              inventoryEvents :: Event t (Vector 46 Slot)
              inventoryEvents = tag (current inventory) (filterCommand "inventory" cmd)

              zipped :: Vector 46 Slot -> Vector 46 (Int, Slot)
              zipped = V.zip (fromJust $ V.fromListN [0..])

              renderSlots :: Vector 46 Slot -> String
              renderSlots = concat . intersperse ", " . catMaybes . fmap (prettySlot . fmap printSlot) . V.toList . zipped

dropCommandE :: Reflex t => Event t ChatCommand -> Event t InventoryAction
dropCommandE cmd = fforMaybe (filterCommand "drop" cmd) $ \c ->
    let args = c ^. arguments
        getArg n = read $ T.unpack (args !! n)
        slot = getArg 0
        --amount = getArg 1
    in  if | length args == 1 -> Just $ DropStack 0 slot
           | otherwise -> Nothing

chatString :: String -> SBPacket
chatString = SBChatMessage . SBChatMessagePayload . view network . T.pack . take 256

printSlot :: Slot -> Maybe String
printSlot (Slot (-1) _ _) = Nothing
printSlot (Slot bid count dmg) = let dmgpart = case dmg of
                                                   Just 0 -> ""
                                                   Just d -> ":" ++ show d
                                                   Nothing -> ""
                                     countpart = case count of
                                                   Just c -> show c ++ "x"
                                                   Nothing -> ""
                                     itempart = case Map.lookup (fromIntegral bid) itemLookup of
                                                  Just name -> name
                                                  Nothing -> show bid
                                 in Just $ countpart ++ itempart ++ dmgpart
