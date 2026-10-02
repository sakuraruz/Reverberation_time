{-# LANGUAGE OverloadedStrings #-}

module Types.Material
    (
      SurfaceMaterial(..)
    , DoorMaterial(..)
    , WindowMaterial(..)
    , DecorMaterial(..)
    , SeatMaterial(..)
    ) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )

import Types.Spectrum

data SurfaceMaterial = SurfaceMaterial
    { surfaceMaterialName :: String
    , materialAbsorption  :: Spectrum
    } deriving (Show, Eq)


instance ToJSON SurfaceMaterial where
    toJSON m = object
        [ "surfaceMaterialName" .= surfaceMaterialName m
        , "materialAbsorption"   .= materialAbsorption m
        ]

instance FromJSON SurfaceMaterial where
    parseJSON = withObject "SurfaceMaterial" $ \o ->
        SurfaceMaterial
            <$> o .: "surfaceMaterialName"
            <*> o .: "materialAbsorption"

data DoorMaterial = DoorMaterial
    { doorName       :: String
    , doorAbsorption :: Spectrum
    } deriving (Show, Eq)


instance ToJSON DoorMaterial where
    toJSON m = object
        [ "doorName"       .= doorName m
        , "doorAbsorption" .= doorAbsorption m
        ]

instance FromJSON DoorMaterial where
    parseJSON = withObject "DoorMaterial" $ \o ->
        DoorMaterial
            <$> o .: "doorName"
            <*> o .: "doorAbsorption"

data WindowMaterial = WindowMaterial
    { windowName       :: String
    , windowAbsorption :: Spectrum
    } deriving (Show, Eq)


instance ToJSON WindowMaterial where
    toJSON m = object
        [ "windowName"       .= windowName m
        , "windowAbsorption" .= windowAbsorption m
        ]

instance FromJSON WindowMaterial where
    parseJSON = withObject "WindowMaterial" $ \o ->
        WindowMaterial
            <$> o .: "windowName"
            <*> o .: "windowAbsorption"

data DecorMaterial = DecorMaterial
    { decorName        :: String
    , decorAbsorption  :: Spectrum
    } deriving (Show, Eq)


instance ToJSON DecorMaterial where
    toJSON m = object
        [ "decorName"        .= decorName m
        , "decorAbsorption"  .= decorAbsorption m
        ]

instance FromJSON DecorMaterial where
    parseJSON = withObject "DecorMaterial" $ \o ->
        DecorMaterial
            <$> o .: "decorName"
            <*> o .: "decorAbsorption"

data SeatMaterial = SeatMaterial
    { seatName             :: String
    , seatEquivalentArea   :: Spectrum
    , seatFloorAreaPerUnit :: Double
    } deriving (Show, Eq)


instance ToJSON SeatMaterial where
    toJSON m = object
        [ "seatName"             .= seatName m
        , "seatEquivalentArea"   .= seatEquivalentArea m
        , "seatFloorAreaPerUnit" .= seatFloorAreaPerUnit m
        ]

instance FromJSON SeatMaterial where
    parseJSON = withObject "SeatMaterial" $ \o ->
        SeatMaterial
            <$> o .: "seatName"
            <*> o .: "seatEquivalentArea"
            <*> o .: "seatFloorAreaPerUnit"