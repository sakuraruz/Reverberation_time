module Evaluation.Suitability
    ( calculateDeviation
    , calculateDeviations
    , calculateRelativeDeviation
    , gradeFromDeviation
    , evaluateSuitability
    ) where

import Types.Spectrum
import Types.Purpose
import Types.AcousticResult
import Types.Suitability

calculateDeviation :: Double -> Range -> Double
calculateDeviation value (Range lo hi)
    | value < lo = lo - value
    | value > hi = value - hi
    | otherwise  = 0

calculateDeviations :: Spectrum -> ReverbSpectrum -> Deviation
calculateDeviations t spectrum =
    Deviation
        { dev125  = calculateDeviation (at125  t) (reverbLow  spectrum)
        , dev500  = calculateDeviation (at500  t) (reverbMid  spectrum)
        , dev2000 = calculateDeviation (at2000 t) (reverbHigh spectrum)
        }

calculateRelativeDeviation :: Spectrum -> ReverbSpectrum -> Double
calculateRelativeDeviation t spectrum =
    maximum
        [ relativeAt (at125  t) (reverbLow  spectrum)
        , relativeAt (at500  t) (reverbMid  spectrum)
        , relativeAt (at2000 t) (reverbHigh spectrum)
        , 0
        ]
  where
    relativeAt value (Range lo hi)
        | value < lo = (lo - value) / lo
        | value > hi = (value - hi) / hi
        | otherwise  = 0

gradeFromDeviation :: Double -> Grade
gradeFromDeviation r
    | r == 0    = Suitable
    | r <= 0.1  = ConditionallySuitable
    | otherwise = Unsuitable

evaluateSuitability
    :: AcousticResult
    -> PurposeReq
    -> SuitabilityResult
evaluateSuitability acoustic purpose =
    SuitabilityResult
        { suitable    = grade == Suitable
        , deviations  = devs
        , relativeDev = relDev
        , grade       = grade
        , reachable   = True
        }
  where
    t        = reverberationTime acoustic
    spectrum = recommendedReverb purpose
    devs     = calculateDeviations t spectrum
    relDev   = calculateRelativeDeviation t spectrum
    grade    = gradeFromDeviation relDev