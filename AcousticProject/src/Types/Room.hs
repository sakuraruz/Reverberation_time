module Types.Room where

import Types.Geometry
import Types.Material

data AdjustableItem = AdjustableItem
    { itemName     :: String
    , itemMaterial :: Material
    , itemArea     :: Double
    } deriving (Show, Eq)

data Room = Room
    { roomName        :: String
    , dimensions      :: Dimensions
    , surfaces        :: [Surface]
    , adjustableItems :: [AdjustableItem]
    } deriving (Show, Eq)