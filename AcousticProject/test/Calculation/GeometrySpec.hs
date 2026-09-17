module Calculation.GeometrySpec where

import Test.QuickCheck

import Types.Geometry
import Calculation.Geometry


prop_volumeNonNegative
    :: Positive Double
    -> Positive Double
    -> Positive Double
    -> Bool
prop_volumeNonNegative (Positive l) (Positive w) (Positive h) =
    calculateVolume (Dimensions l w h) >= 0


prop_volumeZeroWhenLengthZero
    :: Positive Double
    -> Positive Double
    -> Bool
prop_volumeZeroWhenLengthZero (Positive w) (Positive h) =
    calculateVolume (Dimensions 0 w h) == 0


prop_volumeFormula
    :: Positive Double
    -> Positive Double
    -> Positive Double
    -> Bool
prop_volumeFormula (Positive l) (Positive w) (Positive h) =
    calculateVolume (Dimensions l w h)
        == l * w * h


prop_volumeDoesNotDecreaseWhenLengthIncreases
    :: Positive Double
    -> Positive Double
    -> Positive Double
    -> Bool
prop_volumeDoesNotDecreaseWhenLengthIncreases
    (Positive l)
    (Positive w)
    (Positive h) =
        let v1 = calculateVolume (Dimensions l w h)
            v2 = calculateVolume (Dimensions (l + 1) w h)
        in v2 >= v1