module Types.Recommendation where

import Types.Room
import Types.Material
import Types.Event
import Types.AcousticResult
import Types.Suitability

data Recommendation = Recommendation
    { action         :: String
    , materialName   :: String
    , previousReverb :: Double
    , newReverb      :: Double
    , result         :: SuitabilityResult
    } deriving (Show, Eq)