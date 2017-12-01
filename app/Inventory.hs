{-# LANGUAGE DataKinds, ViewPatterns, LambdaCase #-}
module Inventory where

import Control.Lens
import Control.Monad.Fix (MonadFix)
import Data.Finite
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V
import Reflex

import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

data InventoryAction = SetSlot Integer Integer Slot
                     | DropStack Integer Integer
                     deriving (Show)

inventoryD :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t InventoryAction -> m (Dynamic t (Vector 46 Slot))
inventoryD action = foldDynMaybe foldFun (V.replicate emptySlot) action
    where foldFun :: InventoryAction -> V.Vector 46 Slot -> Maybe (Vector 46 Slot)
          foldFun act inv = case act of
                              SetSlot 0 slot slotData -> do
                                  slot' <- packFinite slot
                                  pure $ inv & V.ix slot' .~ slotData
                              DropStack 0 slot -> do
                                  slot' <- packFinite slot
                                  pure $ inv & V.ix slot' .~ emptySlot
                              _ -> Nothing

cbInventoryActionsE :: Reflex t => Event t CBPacket -> Event t InventoryAction
cbInventoryActionsE = fmapMaybe $ \case
    CBSetSlot (CBSetSlotPayload (fromIntegral -> wid) (fromIntegral -> slot) slotData) ->
        Just $ SetSlot wid slot slotData
    _ -> Nothing

data InventoryMap = InventoryMap { inventoryMapNext :: Int16
                                 , inventoryMapMap :: Map Int16 InventoryAction
                                 }

inventoryMapEmpty :: InventoryMap
inventoryMapEmpty = InventoryMap 0 Map.empty

inventoryMapInsert :: InventoryAction -> InventoryMap -> InventoryMap
inventoryMapInsert action (InventoryMap next map) = InventoryMap (next+1) (Map.insert next action map)

inventoryMapDelete :: Int16 -> InventoryMap -> InventoryMap
inventoryMapDelete id (InventoryMap next map) = InventoryMap next (Map.delete id map)

transactionsD :: (Reflex t, MonadHold t m, MonadFix m)
              => Event t (NonEmpty InventoryAction)
              -> Event t Int16
              -> m (Event t [SBPacket], Event t InventoryAction)
transactionsD actions confirmations = do
    let additions = foldr1 (.) . fmap inventoryMapInsert <$> actions
        deletions = inventoryMapDelete <$> confirmations
        mapActions = mergeWith (.) [additions, deletions]
        addedTransactions = ($ inventoryMapEmpty) <$> additions
    transactions <- fmap inventoryMapMap <$> foldDyn id inventoryMapEmpty mapActions
    let localInventoryAction = attachWithMaybe (flip Map.lookup) (current transactions) confirmations
    pure (transactionPacketsE addedTransactions, localInventoryAction)


transactionPacketsE :: Reflex t => Event t InventoryMap -> Event t [SBPacket]
transactionPacketsE actions = ffor actions $ catMaybes . fmap mkPacket . Map.toList . inventoryMapMap
    where mkPacket :: (Int16, InventoryAction) -> Maybe SBPacket
          mkPacket (tnum, action) = case action of
                                      SetSlot _ _ _ -> Nothing
                                      DropStack (fromIntegral -> wid) (fromIntegral -> slot) ->
                                          Just $ SBClickWindow (SBClickWindowPayload wid slot 1 tnum 4 (Slot (-1) Nothing Nothing))

transactionConfirmationsE :: Reflex t => Event t CBPacket -> Event t Int16
transactionConfirmationsE inbound = fforMaybe inbound $ \case
    CBConfirmTransaction (CBConfirmTransactionPayload 0 i True) -> Just i
    _ -> Nothing
