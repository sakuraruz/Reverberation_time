module Types.Material where

data Material = Material
    { materialName          :: String
    , absorptionCoefficient :: Double
    , removable             :: Bool
    , averageArea           :: Maybe Double
    } deriving (Show, Eq)

data Surface = Surface
    { surfaceMaterial :: Material
    , surfaceArea     :: Double
    } deriving (Show, Eq)