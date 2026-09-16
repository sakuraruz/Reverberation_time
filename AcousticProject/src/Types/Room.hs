{-# LANGUAGE OverloadedStrings #-}

module Types.Room where

import Types.Geometry
import Types.Material

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )

data AdjustableItem = AdjustableItem
    { itemName     :: String
    , itemMaterial :: Material
    , itemArea     :: Double
    }
    deriving (Show, Eq)

data Room = Room
    { roomName        :: String
    , dimensions      :: Dimensions
    , surfaces        :: [Surface]
    , adjustableItems :: [AdjustableItem]
    }
    deriving (Show, Eq)


instance ToJSON AdjustableItem where
    toJSON item =
        object
            [ "itemName"     .= itemName item
            , "itemMaterial" .= itemMaterial item
            , "itemArea"     .= itemArea item
            ]

instance FromJSON AdjustableItem where
    parseJSON = withObject "AdjustableItem" $ \object ->
        AdjustableItem
            <$> object .: "itemName"
            <*> object .: "itemMaterial"
            <*> object .: "itemArea"


instance ToJSON Room where
    toJSON room =
        object
            [ "roomName"        .= roomName room
            , "dimensions"      .= dimensions room
            , "surfaces"        .= surfaces room
            , "adjustableItems" .= adjustableItems room
            ]

instance FromJSON Room where
    parseJSON = withObject "Room" $ \object ->
        Room
            <$> object .: "roomName"
            <*> object .: "dimensions"
            <*> object .: "surfaces"
            <*> object .: "adjustableItems"