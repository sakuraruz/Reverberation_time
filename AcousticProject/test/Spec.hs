module Main where

import Test.QuickCheck

import qualified Calculation.GeometrySpec as Geometry
import qualified Calculation.AbsorptionSpec as Absorption
import qualified Calculation.ReverberationSpec as Reverberation
import qualified Evaluation.SuitabilitySpec as Suitability
import qualified Recommendation.AcousticSpec as Recommendation


main :: IO ()
main = do

    putStrLn "=== Calculation.Geometry ==="

    quickCheck Geometry.prop_volumeNonNegative
    quickCheck Geometry.prop_volumeZeroWhenLengthZero
    quickCheck Geometry.prop_volumeFormula
    quickCheck Geometry.prop_volumeDoesNotDecreaseWhenLengthIncreases


    putStrLn "\n=== Calculation.Absorption ==="

    quickCheck Absorption.prop_surfaceAbsorptionNonNegative
    quickCheck Absorption.prop_surfaceAbsorptionFormula
    quickCheck Absorption.prop_itemAbsorptionNonNegative
    quickCheck Absorption.prop_itemAbsorptionFormula
    quickCheck Absorption.prop_roomAbsorptionNonNegative


    putStrLn "\n=== Calculation.Reverberation ==="

    quickCheck Reverberation.prop_reverberationNonNegative
    quickCheck Reverberation.prop_zeroAbsorptionGivesZero
    quickCheck Reverberation.prop_negativeAbsorptionGivesZero
    quickCheck Reverberation.prop_moreAbsorptionDoesNotIncreaseReverberation


    putStrLn "\n=== Evaluation.Suitability ==="

    quickCheck Suitability.prop_deviationIsZeroInsideRange
    quickCheck Suitability.prop_deviationBelowMinimum
    quickCheck Suitability.prop_deviationAboveMaximum
    quickCheck Suitability.prop_valuesInsideRangeAreAccepted
    quickCheck Suitability.prop_minimumBoundaryIsAccepted
    quickCheck Suitability.prop_maximumBoundaryIsAccepted
    quickCheck Suitability.prop_valuesBelowRangeAreRejected
    quickCheck Suitability.prop_valuesAboveRangeAreRejected
    quickCheck Suitability.prop_suitableRoomIsMarkedSuitable
    quickCheck Suitability.prop_belowRangeIsUnsuitable
    quickCheck Suitability.prop_aboveRangeIsUnsuitable


    putStrLn "\n=== Recommendation.Acoustic ==="

    quickCheck Recommendation.prop_suitableRoomHasNoRecommendations
    quickCheck Recommendation.prop_noItemsMeansNoRecommendations
    quickCheck Recommendation.prop_highReverberationProducesRecommendations
    quickCheck Recommendation.prop_lowReverberationProducesRecommendations
    quickCheck Recommendation.prop_normalReverberationProducesNoRecommendations
    quickCheck Recommendation.prop_highReverberationRecommendsAdding
    quickCheck Recommendation.prop_lowReverberationRecommendsRemoving
    quickCheck Recommendation.prop_recommendationChangesReverberation
    quickCheck Recommendation.prop_recommendationContainsMaterialName