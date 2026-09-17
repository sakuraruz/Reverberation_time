{-# LANGUAGE OverloadedStrings #-}

module Types.Material where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )

data Material = Material
    { materialName          :: String
    , absorptionCoefficient :: Double
    , removable             :: Bool
    }
    deriving (Show, Eq)

data Surface = Surface
    { surfaceMaterial :: Material
    , surfaceArea     :: Double
    }
    deriving (Show, Eq)

newtype YesNo = YesNo
    { unYesNo :: Bool
    }
    deriving (Show, Eq)


instance ToJSON Material where
    toJSON material =
        object
            [ "materialName"          .= materialName material
            , "absorptionCoefficient" .= absorptionCoefficient material
            , "removable"             .= removable material
            ]

instance FromJSON Material where
    parseJSON = withObject "Material" $ \object ->
        Material
            <$> object .: "materialName"
            <*> object .: "absorptionCoefficient"
            <*> object .: "removable"


instance ToJSON Surface where
    toJSON surface =
        object
            [ "surfaceMaterial" .= surfaceMaterial surface
            , "surfaceArea"     .= surfaceArea surface
            ]

instance FromJSON Surface where
    parseJSON = withObject "Surface" $ \object ->
        Surface
            <$> object .: "surfaceMaterial"
            <*> object .: "surfaceArea"