module Calculation.Geometry where

import Types.Geometry

calculateVolume :: Dimensions -> Double
calculateVolume dimensions =
    roomLength dimensions
        * roomWidth dimensions
        * roomHeight dimensions