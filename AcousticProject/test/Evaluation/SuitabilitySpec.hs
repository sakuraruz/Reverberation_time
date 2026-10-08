module Evaluation.SuitabilitySpec where

import Test.QuickCheck

import Types.Spectrum
import Types.Suitability (Grade(..))
import Evaluation.Suitability


instance Arbitrary Range where
    arbitrary = do
        lo <- choose (0.1, 1.0)
        hi <- choose (lo, 2.0)
        pure (Range lo hi)


prop_deviationNonNegative :: Double -> Range -> Bool
prop_deviationNonNegative value r =
    calculateDeviation value r >= 0


prop_deviationZeroInRange :: Range -> Bool
prop_deviationZeroInRange (Range lo hi) =
    let mid = (lo + hi) / 2
    in calculateDeviation mid (Range lo hi) == 0


prop_gradeSuitableWhenZero :: Bool
prop_gradeSuitableWhenZero =
    gradeFromDeviation 0 == Suitable