module Types.Recommendation
    (Action(..)
    , Recommendation(..)
    ) where

import Types.Spectrum
import Types.Material
import Types.Room
import Types.Suitability

data Action
    = Add
    | Remove
    deriving (Show, Eq)

data Recommendation = Recommendation
    { action          :: Action
    , targetElement   :: DecorMaterial
    , targetSurface   :: SurfaceId
    , recommendedArea :: Double
    , previousReverb  :: Spectrum
    , newReverb       :: Spectrum
    , newSuitability  :: SuitabilityResult
    } deriving (Show, Eq)