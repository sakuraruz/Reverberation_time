module Calculation.ReverberationSpec where

import Test.QuickCheck

import Calculation.Reverberation

prop_sabineNonNegative :: Positive Double -> Positive Double -> Bool
prop_sabineNonNegative (Positive v) (Positive a) =
    calculateSabine v a >= 0


prop_sabineZeroWhenAbsorptionZero :: Positive Double -> Bool
prop_sabineZeroWhenAbsorptionZero (Positive v) =
    calculateSabine v 0 == 0


prop_sabineFormula :: Positive Double -> Positive Double -> Bool
prop_sabineFormula (Positive v) (Positive a) =
    abs (calculateSabine v a - 0.163 * v / a) < 0.0001


prop_sabineProportionalToVolume :: Positive Double -> Positive Double -> Bool
prop_sabineProportionalToVolume (Positive v) (Positive a) =
    let t1 = calculateSabine v a
        t2 = calculateSabine (2 * v) a
    in abs (t2 - 2 * t1) < 0.0001

prop_eyringNonNegative :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_eyringNonNegative (Positive v) (Positive s) (Positive phi) =
    calculateEyring v s phi >= 0


prop_eyringZeroWhenCommonAreaZero :: Positive Double -> Positive Double -> Bool
prop_eyringZeroWhenCommonAreaZero (Positive v) (Positive phi) =
    calculateEyring v 0 phi == 0


prop_eyringZeroWhenPhiZero :: Positive Double -> Positive Double -> Bool
prop_eyringZeroWhenPhiZero (Positive v) (Positive s) =
    calculateEyring v s 0 == 0


prop_eyringFormula :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_eyringFormula (Positive v) (Positive s) (Positive phi) =
    abs (calculateEyring v s phi - 0.163 * v / (s * phi)) < 0.0001


prop_eyringProportionalToVolume :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_eyringProportionalToVolume (Positive v) (Positive s) (Positive phi) =
    let t1 = calculateEyring v s phi
        t2 = calculateEyring (2 * v) s phi
    in abs (t2 - 2 * t1) < 0.0001


prop_eyringInverselyProportionalToCommonArea :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_eyringInverselyProportionalToCommonArea (Positive v) (Positive s) (Positive phi) =
    let t1 = calculateEyring v s phi
        t2 = calculateEyring v (2 * s) phi
    in abs (t2 - t1 / 2) < 0.0001


prop_eyringInverselyProportionalToPhi :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_eyringInverselyProportionalToPhi (Positive v) (Positive s) (Positive phi) =
    let t1 = calculateEyring v s phi
        t2 = calculateEyring v s (2 * phi)
    in abs (t2 - t1 / 2) < 0.0001