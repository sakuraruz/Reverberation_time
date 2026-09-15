module Calculation.Reverberation where

import Types.Geometry

calculateReverberationTime :: Volume -> Double -> Double
calculateReverberationTime volume absorption
    | absorption <= 0 = 0
    | otherwise = 0.161 * volume / absorption