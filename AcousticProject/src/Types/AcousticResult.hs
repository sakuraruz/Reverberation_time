module Types.AcousticResult
    ( AcousticResult(..)
    ) where

import Types.Spectrum

data AcousticResult = AcousticResult
    { roomVolume        :: Double
    , commonArea        :: Double
    , totalAbsorption   :: Spectrum
    , reverberationTime :: Spectrum
    } deriving (Show, Eq)