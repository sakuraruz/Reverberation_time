module Calculation.ReverberationSpec where

import Test.QuickCheck

import Calculation.Reverberation


prop_reverberationNonNegative
    :: Positive Double
    -> Positive Double
    -> Bool
prop_reverberationNonNegative
    (Positive volume)
    (Positive absorption) =
        calculateReverberationTime volume absorption >= 0


prop_zeroAbsorptionGivesZero
    :: Positive Double
    -> Bool
prop_zeroAbsorptionGivesZero (Positive volume) =
    calculateReverberationTime volume 0 == 0


prop_negativeAbsorptionGivesZero
    :: Positive Double
    -> Bool
prop_negativeAbsorptionGivesZero (Positive volume) =
    calculateReverberationTime volume (-1) == 0


prop_moreAbsorptionDoesNotIncreaseReverberation
    :: Positive Double
    -> Positive Double
    -> Bool
prop_moreAbsorptionDoesNotIncreaseReverberation
    (Positive volume)
    (Positive absorption) =

        let t1 =
                calculateReverberationTime
                    volume
                    absorption

            t2 =
                calculateReverberationTime
                    volume
                    (absorption + 1)

        in t2 <= t1