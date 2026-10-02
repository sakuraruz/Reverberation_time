module Calculation.Geometry
    ( calculateVolume
    , calculateFloorArea
    , calculateTotalSurfaceArea
    , calculateSeatsArea
    , calculateCommonArea
    , calculateFreeArea
    , validateSeatsArea
    , validateFreeArea
    ) where

import Types.Geometry
import Types.Room
import Types.Material

calculateVolume :: Dimensions -> Double
calculateVolume d =
    roomLength d * roomWidth d * roomHeight d
    
calculateFloorArea :: Dimensions -> Double
calculateFloorArea d =
    roomLength d * roomWidth d

calculateTotalSurfaceArea :: Dimensions -> Double
calculateTotalSurfaceArea d =
    2 * ( roomLength d * roomWidth d
        + roomLength d * roomHeight d
        + roomWidth  d * roomHeight d
        )

calculateSeatsArea :: [SeatItem] -> Double
calculateSeatsArea seats =
    sum [ fromIntegral (seatCount s)
          * seatFloorAreaPerUnit (seatMaterial s)
        | s <- seats ]

calculateCommonArea :: Dimensions -> [SeatItem] -> Double
calculateCommonArea d seats =
    calculateTotalSurfaceArea d - calculateSeatsArea seats

calculateFreeArea :: Surface -> [SeatItem] -> Double
calculateFreeArea surface seats =
    let baseArea      = surfaceArea surface
        elemsArea     = sum (map elemArea (surfaceElems surface))
        seatsOnFloor  = case surfaceId surface of
                          Floor -> calculateSeatsArea seats
                          _     -> 0
    in baseArea - elemsArea - seatsOnFloor

elemArea :: SurfaceElem -> Double
elemArea (ElemDoor d)   = doorArea d
elemArea (ElemWindow w) = windowArea w
elemArea (ElemDecor d)  = decorArea d

validateSeatsArea :: Dimensions -> [SeatItem] -> Either String ()
validateSeatsArea d seats
    | calculateSeatsArea seats > calculateFloorArea d =
        Left "Площадь зоны сидячих мест превышает площадь пола"
    | otherwise = Right ()

validateFreeArea :: Surface -> [SeatItem] -> Either String ()
validateFreeArea surface seats
    | calculateFreeArea surface seats < 0 =
        Left ("Свободная площадь поверхности отрицательна: "
              ++ show (surfaceId surface))
    | otherwise = Right ()

