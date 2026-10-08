module Calculation.GeometrySpec where

import Test.QuickCheck

import Types.Geometry
import Calculation.Geometry


prop_volumeNonNegative :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_volumeNonNegative (Positive l) (Positive w) (Positive h) =
    calculateVolume (Dimensions l w h) >= 0


prop_volumeFormula :: Positive Double-> Positive Double -> Positive Double -> Bool
prop_volumeFormula (Positive l) (Positive w) (Positive h) =
    calculateVolume (Dimensions l w h) == l * w * h


prop_totalSurfaceAreaGreaterThanFloor :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_totalSurfaceAreaGreaterThanFloor (Positive l) (Positive w) (Positive h) =
    let d = Dimensions l w h
    in calculateTotalSurfaceArea d > calculateFloorArea d