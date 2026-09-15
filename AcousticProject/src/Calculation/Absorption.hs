module Calculation.Absorption where

import Types.Material
import Types.Room

calculateSurfaceAbsorption :: Surface -> Double
calculateSurfaceAbsorption surface =
    absorptionCoefficient (surfaceMaterial surface)
        * surfaceArea surface

calculateItemAbsorption :: AdjustableItem -> Double
calculateItemAbsorption item =
    absorptionCoefficient (itemMaterial item)
        * itemArea item

calculateAbsorptionArea :: Room -> Double
calculateAbsorptionArea room =
    surfaceAbsorption + itemAbsorption
  where
    surfaceAbsorption =
        sum
            (map calculateSurfaceAbsorption
                (surfaces room))

    itemAbsorption =
        sum
            (map calculateItemAbsorption
                (adjustableItems room))