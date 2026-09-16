module Evaluation.Suitability where

import Types.AcousticResult
import Types.Event
import Types.Suitability

calculateDeviation :: Double -> (Double, Double) -> Double
calculateDeviation value (minValue, maxValue)
    | value < minValue = minValue - value
    | value > maxValue = value - maxValue
    | otherwise = 0

isWithinRange :: Double -> (Double, Double) -> Bool
isWithinRange value (minValue, maxValue) =
    value >= minValue && value <= maxValue

evaluateSuitability :: AcousticResult -> EventRequirement -> SuitabilityResult
evaluateSuitability acoustic event =
    SuitabilityResult
        { suitable =
            isWithinRange
                (reverberationTime acoustic)
                (recommendedReverb event)

        , deviation =
            calculateDeviation
                (reverberationTime acoustic)
                (recommendedReverb event)
        }