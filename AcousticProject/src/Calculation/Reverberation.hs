module Calculation.Reverberation
    (calculateSabine
    , calculateEyring
    , calculateReverberationTime
    , calculateReverberationSpectrum
    ) where

import Types.Spectrum
import Types.AcousticResult

sabineConstant :: Double
sabineConstant = 0.163

calculateSabine :: Double -> Double -> Double
calculateSabine volume totalAbsorption
    | totalAbsorption <= 0 = 0
    | otherwise            = sabineConstant * volume / totalAbsorption

calculateEyring :: Double -> Double -> Double -> Double
calculateEyring volume commonArea phi
    | commonArea <= 0 = 0
    | phi <= 0        = 0
    | otherwise       = sabineConstant * volume / (commonArea * phi)

calculateReverberationTime
    :: Double
    -> Double
    -> Double
    -> Double
calculateReverberationTime volume totalAbsorption commonArea
    | totalAbsorption <= 0 = 0
    | commonArea <= 0      = 0
    | alphaAvg <= 0.2      = calculateSabine volume totalAbsorption
    | alphaAvg >= 1.0      = 0
    | otherwise            = calculateEyring volume commonArea phi
  where
    alphaAvg = totalAbsorption / commonArea
    phi      = negate (log (1 - alphaAvg))

calculateReverberationSpectrum
    :: Double
    -> Spectrum
    -> Double
    -> Spectrum
calculateReverberationSpectrum volume absorptions commonArea =
    Spectrum
        { at125  = calculateReverberationTime volume (at125  absorptions) commonArea
        , at500  = calculateReverberationTime volume (at500  absorptions) commonArea
        , at2000 = calculateReverberationTime volume (at2000 absorptions) commonArea
        }