module Calculation.Absorption
    ( calculateSurfaceAbsorption
    , calculateSurfacesAbsorption
    , calculateSeatsAbsorption
    , calculateAdditionalAbsorption
    , additionalAlpha
    , calculateTotalAbsorption
    ) where

import Types.Spectrum
import Types.Material
    ( SurfaceMaterial(..)
    , DoorMaterial(..)
    , WindowMaterial(..)
    , DecorMaterial(..)
    , SeatMaterial(..)
    )
import Types.Room
import Types.Geometry

import Calculation.Geometry (calculateFreeArea)

calculateSurfaceAbsorption :: Surface -> [SeatItem] -> Spectrum
calculateSurfaceAbsorption surface seats =
    addSpectrums basePart elemPart
  where
    freeArea  = calculateFreeArea surface seats
    baseAlpha = materialAbsorption (surfaceMaterial surface)
    basePart  = multiplySpectrum baseAlpha freeArea
    elemPart  = sumSpectrums (map elemAbsorption (surfaceElems surface))

    elemAbsorption (ElemDoor d) =
        multiplySpectrum (doorAbsorption (doorMaterial d)) (doorArea d)
    elemAbsorption (ElemWindow w) =
        multiplySpectrum (windowAbsorption (windowMaterial w)) (windowArea w)
    elemAbsorption (ElemDecor d) =
        multiplySpectrum (decorAbsorption (decorMaterial d)) (decorArea d)

calculateSurfacesAbsorption :: [Surface] -> [SeatItem] -> Spectrum
calculateSurfacesAbsorption surfaces seats =
    sumSpectrums
        [ calculateSurfaceAbsorption s seats
        | s <- surfaces ]

calculateSeatsAbsorption :: [SeatItem] -> Spectrum
calculateSeatsAbsorption seats =
    sumSpectrums
        [ multiplySpectrum
            (seatEquivalentArea (seatMaterial s))
            (fromIntegral (seatCount s))
        | s <- seats ]

additionalAlpha :: Spectrum
additionalAlpha = Spectrum
    { at125  = 0.0425
    , at500  = 0.0225
    , at2000 = 0.0225
    }

calculateAdditionalAbsorption :: Double -> Spectrum
calculateAdditionalAbsorption commonArea =
    multiplySpectrum additionalAlpha commonArea

calculateTotalAbsorption
    :: [Surface]
    -> [SeatItem]
    -> Double
    -> Spectrum
calculateTotalAbsorption surfaces seats commonArea =
    addSpectrums
        (addSpectrums
            (calculateSurfacesAbsorption surfaces seats)
            (calculateSeatsAbsorption seats))
        (calculateAdditionalAbsorption commonArea)