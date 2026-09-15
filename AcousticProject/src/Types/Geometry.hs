module Types.Geometry where

data Dimensions = Dimensions
    { roomLength :: Double
    , roomWidth  :: Double
    , roomHeight :: Double
    } deriving (Show, Eq)