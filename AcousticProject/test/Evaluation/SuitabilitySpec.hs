module Evaluation.SuitabilitySpec where

import Test.QuickCheck

import Types.AcousticResult
import Types.Event
import Types.Suitability
import Evaluation.Suitability


lecture :: EventRequirement
lecture =
    EventRequirement
        { eventType = EventType "Лекция"
        , recommendedReverb = (0.8, 1.2)
        }


prop_deviationIsZeroInsideRange
    :: Positive Double
    -> Bool
prop_deviationIsZeroInsideRange (Positive x) =
    let value =
            0.8
                + (x / (1 + x))
                * 0.4

    in calculateDeviation value (0.8, 1.2) == 0


prop_deviationBelowMinimum
    :: Positive Double
    -> Bool
prop_deviationBelowMinimum (Positive x) =
    let value =
            0.8 * x / (1 + x)

        expected =
            0.8 - value

        actual =
            calculateDeviation value (0.8, 1.2)

    in abs (actual - expected) < 1e-9


prop_deviationAboveMaximum
    :: Positive Double
    -> Bool
prop_deviationAboveMaximum (Positive x) =
    let value =
            1.2 + x

        expected =
            value - 1.2

        actual =
            calculateDeviation value (0.8, 1.2)

    in abs (actual - expected) < 1e-9


prop_valuesInsideRangeAreAccepted
    :: Positive Double
    -> Bool
prop_valuesInsideRangeAreAccepted (Positive x) =
    let value =
            0.8
                + (x / (1 + x))
                * 0.4

    in isWithinRange value (0.8, 1.2)


prop_minimumBoundaryIsAccepted :: Bool
prop_minimumBoundaryIsAccepted =
    isWithinRange 0.8 (0.8, 1.2)


prop_maximumBoundaryIsAccepted :: Bool
prop_maximumBoundaryIsAccepted =
    isWithinRange 1.2 (0.8, 1.2)


prop_valuesBelowRangeAreRejected
    :: Positive Double
    -> Bool
prop_valuesBelowRangeAreRejected (Positive x) =
    let value = 0.8 - x
    in not (isWithinRange value (0.8, 1.2))


prop_valuesAboveRangeAreRejected
    :: Positive Double
    -> Bool
prop_valuesAboveRangeAreRejected (Positive x) =
    let value = 1.2 + x
    in not (isWithinRange value (0.8, 1.2))


prop_suitableRoomIsMarkedSuitable
    :: Positive Double
    -> Bool
prop_suitableRoomIsMarkedSuitable (Positive x) =
    let value =
            0.8
                + (x / (1 + x))
                * 0.4

        acoustic =
            AcousticResult
                { roomVolume = 60
                , absorptionArea = 10
                , reverberationTime = value
                }

        result =
            evaluateSuitability acoustic lecture

    in suitable result
        && deviation result == 0


prop_belowRangeIsUnsuitable
    :: Positive Double
    -> Bool
prop_belowRangeIsUnsuitable (Positive x) =
    let value = 0.8 - x

        acoustic =
            AcousticResult
                { roomVolume = 60
                , absorptionArea = 10
                , reverberationTime = value
                }

        result =
            evaluateSuitability acoustic lecture

    in not (suitable result)
        && deviation result > 0


prop_aboveRangeIsUnsuitable
    :: Positive Double
    -> Bool
prop_aboveRangeIsUnsuitable (Positive x) =
    let value = 1.2 + x

        acoustic =
            AcousticResult
                { roomVolume = 60
                , absorptionArea = 10
                , reverberationTime = value
                }

        result =
            evaluateSuitability acoustic lecture

    in not (suitable result)
        && deviation result > 0