{-# LANGUAGE DataKinds, ViewPatterns, LambdaCase #-}
module Inventory where

import Control.Lens
import Control.Monad.Fix (MonadFix)
import Data.Finite
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V
import Reflex

import Network.Protocol.Minecraft.Packet
import Network.Protocol.Minecraft.Types

data InventoryAction = SetSlot Integer Integer Slot

inventoryD :: (Reflex t, MonadHold t m, MonadFix m)
           => Event t InventoryAction -> m (Dynamic t (Vector 46 Slot))
inventoryD action = foldDynMaybe foldFun (V.replicate emptySlot) action
    where foldFun :: InventoryAction -> V.Vector 46 Slot -> Maybe (Vector 46 Slot)
          foldFun act inv = case act of
                              SetSlot 0 slot slotData | slot <= 45 -> do
                                  slot' <- packFinite slot
                                  pure $ inv & V.ix slot' .~ slotData
                              SetSlot _ _ _ -> Nothing

setSlotE :: Reflex t => Event t CBPacket -> Event t InventoryAction
setSlotE = fmapMaybe $ \case
    CBSetSlot (CBSetSlotPayload (fromIntegral -> wid) (fromIntegral -> slot) slotData) ->
        Just $ SetSlot wid slot slotData
    _ -> Nothing
