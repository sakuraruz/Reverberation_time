module Batch.Check
    ( checkAllRooms
    , checkOneRoom
    , validateRoomForBatch
    ) where

import Control.Monad (forM)
import Data.Either (isLeft)

import Types.AppError
import Types.Room
import Types.Purpose
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation
import Types.Report
import Types.Geometry
import Types.Material (DecorMaterial(..))
import Input.CSV (loadWallDecor, loadFloorDecor)
import Storage.JSON (loadRooms)

import Calculation.Geometry
import Calculation.Absorption
import Calculation.Reverberation
import Evaluation.Suitability
import Recommendation.Acoustic

import Log.Logger

checkAllRooms :: FilePath -> PurposeReq -> FilePath -> FilePath -> FilePath -> IO (Either AppError BatchResult)
checkAllRooms roomsFile purpose wallFile floorFile logPath = do
    roomsResult <- loadRooms roomsFile
    case roomsResult of
        Left err -> pure (Left err)
        Right rooms -> do
            wallResult <- loadWallDecor wallFile
            case wallResult of
                Left err -> pure (Left err)
                Right wallCatalog -> do

                    floorResult <- loadFloorDecor floorFile
                    case floorResult of
                        Left err -> pure (Left err)
                        Right floorCatalog -> do

                            entries <- forM rooms $ \room -> do
                                case validateRoomForBatch room of

                                    Left reason -> do
                                        _ <- logWarning logPath
                                                ("Пропущено: " ++ roomName room
                                                 ++ " — " ++ reason)
                                        pure (BatchSkipped (roomName room) reason)

                                    Right () ->
                                        case checkOneRoom
                                                wallCatalog
                                                floorCatalog
                                                room
                                                purpose of

                                            Left err -> do
                                                _ <- logError logPath
                                                        ("Ошибка проверки: "
                                                         ++ roomName room
                                                         ++ " — " ++ show err)
                                                pure (BatchSkipped (roomName room)
                                                        (show err))

                                            Right report -> do
                                                _ <- logInfo logPath
                                                        ("Обработано: "
                                                         ++ roomName room)
                                                pure (BatchSuccess (roomName room)
                                                        report)

                            let successCount =
                                    length [e | e@BatchSuccess{} <- entries]
                                skippedCount =
                                    length [e | e@BatchSkipped{} <- entries]
                                result = BatchResult
                                    { brTotal     = length rooms
                                    , brProcessed = successCount
                                    , brSkipped   = skippedCount
                                    , brEntries   = entries
                                    }
                            pure (Right result)

checkOneRoom
    :: [DecorMaterial]
    -> [DecorMaterial]
    -> Room
    -> PurposeReq
    -> Either AppError Report
checkOneRoom wallCat floorCat room purpose =
    let volume       = calculateVolume (roomDimensions room)
        commonArea   = calculateCommonArea (roomDimensions room) (roomSeats room)
        absorption   = calculateTotalAbsorption
                          (roomSurfaces room) (roomSeats room) commonArea
        reverb       = calculateReverberationSpectrum volume absorption commonArea
        acoustic     = AcousticResult
            { roomVolume        = volume
            , commonArea        = commonArea
            , totalAbsorption   = absorption
            , reverberationTime = reverb
            }
        suitability  = evaluateSuitability acoustic purpose

        recommendations = generateRecommendations
                            room purpose acoustic suitability
                            wallCat floorCat

        newRoom        = applyRecommendations recommendations room
        newAcoustic    = recalculateAcoustic newRoom
        newSuitability = evaluateSuitability newAcoustic purpose
    in Right Report
        { reportRoom              = room
        , reportPurpose           = purpose
        , reportAcousticBefore    = acoustic
        , reportSuitabilityBefore = suitability
        , reportAcousticAfter     = newAcoustic
        , reportSuitabilityAfter  = newSuitability
        , reportRecommendations   = recommendations
        }

validateRoomForBatch :: Room -> Either String ()
validateRoomForBatch room
    | roomLength d <= 0 || roomWidth d <= 0 || roomHeight d <= 0 =
        Left "размеры <= 0"
    | length (roomSurfaces room) /= 6 =
        Left "должно быть 6 поверхностей"
    | any ((<= 0) . surfaceArea) (roomSurfaces room) =
        Left "площадь поверхности <= 0"
    | isLeft (validateSeatsArea d (roomSeats room)) =
        Left "площадь зоны мест превышает площадь пола"
    | any (not . validateSurface) (roomSurfaces room) =
        Left "отрицательная свободная площадь поверхности"
    | otherwise = Right ()
  where
    d = roomDimensions room

    validateSurface s =
        case validateFreeArea s (roomSeats room) of
            Left _  -> False
            Right _ -> True
