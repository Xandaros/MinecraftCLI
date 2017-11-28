{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Commands where

import Control.Lens
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex

import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

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
           , "help, ping, quit, tp, where, inventory"
           -- , "To get more information about a specific command, use \"help <command>\""
           ]

commandMessagesE :: Reflex t => Event t ChatCommand -> Event t (NonEmpty Text)
commandMessagesE cmd =
    let a = mergeList
                [ checkArguments [3,5] "Invalid arguments. tp <x> <y> <z> [<pitch> <yaw>]" (filterCommand "tp" cmd) 
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

inventoryCommandE :: Reflex t => Event t ChatCommand -> Dynamic t (Map Int Slot) -> Event t SBPacket
inventoryCommandE cmd inventory = chatString . concat . intersperse ", " . fmap (prettySlot . fmap printSlot) . M.toList <$>
    tag (current inventory) (filterCommand "inventory" cmd)
        where prettySlot :: (Int, String) -> String
              prettySlot (slot, slotData) = slotData ++ "(" ++ show slot ++ ")"

chatString :: String -> SBPacket
chatString = SBChatMessage . SBChatMessagePayload . view network . T.pack . take 256
