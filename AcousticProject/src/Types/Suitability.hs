module Types.Suitability where

data SuitabilityResult = SuitabilityResult
    { suitable  :: Bool
    , deviation :: Double
    } deriving (Show, Eq)