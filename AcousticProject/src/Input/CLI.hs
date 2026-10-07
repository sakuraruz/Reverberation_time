module Input.CLI
    ( mainMenu
    ) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Types.AppError
import Types.Spectrum
import Types.Material
import Types.Room
import Types.Geometry
import Types.Purpose
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation
import Types.Report

import Input.CSV
import Storage.JSON
import Output.Report
import Batch.Check

import Calculation.Geometry
import Calculation.Absorption
import Calculation.Reverberation

import Evaluation.Suitability

import Recommendation.Acoustic

import Log.Logger

materialsDir :: FilePath
materialsDir = "data"

logFile :: FilePath
logFile = "data/app.log"

reportDir :: FilePath
reportDir = "data/reports"

summaryReport :: FilePath
summaryReport = "data/summary.txt"

roomsDir :: FilePath
roomsDir = "data/rooms"

mainMenu :: IO (Either AppError ())
mainMenu = do

    putStrLn ""
    putStrLn "========================================"
    putStrLn "   АКУСТИЧЕСКАЯ ОЦЕНКА ПОМЕЩЕНИЯ"
    putStrLn "========================================"
    putStrLn "1. Создать и проверить помещение"
    putStrLn "2. Создать и сохранить помещение"
    putStrLn "3. Пакетная проверка из файла"
    putStrLn "4. Просмотреть сохранённые помещения"
    putStrLn "5. Удалить помещение"
    putStrLn "0. Выход"
    putStrLn "========================================"
    putStr "Выбор: "
    hFlush stdout

    input <- getLine
    case input of
        "1" -> do
            result <- createAndCheck
            handleResult result
            mainMenu
        "2" -> do
            result <- createAndSave
            handleResult result
            mainMenu
        "3" -> do
            result <- batchCheck
            handleResult result
            mainMenu
        "4" -> do
            fileResult <- chooseExistingFileInDir roomsDir
            case fileResult of
                Left (InvalidInput _) -> do
                    putStrLn "Операция отменена."
                    mainMenu
                Left err -> do
                    printError err
                    mainMenu
                Right file -> do
                    _ <- showRooms file
                    mainMenu
        "5" -> do
            fileResult <- chooseExistingFileInDir roomsDir
            case fileResult of
                Left _ -> mainMenu
                Right file -> do
                    _ <- deleteRoomMenu file
                    mainMenu
        "0" -> do
            pure (Right ())
        _ -> do
            putStrLn "Некорректный пункт меню."
            _ <- logWarning logFile ("Некорректный пункт: " ++ input)
            mainMenu

createAndCheck :: IO (Either AppError ())
createAndCheck = do
    roomResult <- createRoom
    case roomResult of
        Left err -> pure (Left err)
        Right room -> do
            purposeResult <- readPurpose
            case purposeResult of
                Left err -> pure (Left err)
                Right purpose -> do
                    checkRoom room purpose
                    pure (Right ())

createAndSave :: IO (Either AppError ())
createAndSave = do
    roomResult <- createRoom
    case roomResult of
        Left err -> pure (Left err)
        Right room -> do
            fileResult <- chooseOrCreateFileInDir roomsDir
            case fileResult of
                Left err -> pure (Left err)
                Right file -> do
                    result <- addRoom file room False
                    case result of
                        Left (InvalidInput msg) -> do
                            putStrLn msg
                            putStr "Перезаписать? (y/n): "
                            hFlush stdout
                            answer <- getLine
                            if answer == "y"
                                then addRoom file room True
                                else pure (Right ())
                        other -> pure other

batchCheck :: IO (Either AppError ())
batchCheck = do
    fileResult <- chooseExistingFileInDir roomsDir
    case fileResult of
        Left _ -> mainMenu
        Right file -> do

            purposeResult <- readPurpose
            case purposeResult of
                Left err -> do
                    printError err
                    mainMenu
                Right purpose -> do

                    result <- checkAllRooms
                                file
                                purpose
                                (materialsDir ++ "/wall_decor.csv")
                                (materialsDir ++ "/flour_carpet.csv")
                                logFile
                    case result of
                        Left err -> pure (Left err)
                        Right batchResult -> do
                            putStrLn ""
                            putStrLn $ "Всего помещений: " ++ show (brTotal batchResult)
                            putStrLn $ "Обработано: "      ++ show (brProcessed batchResult)
                            putStrLn $ "Пропущено: "       ++ show (brSkipped batchResult)

                            saveResult <- saveSummaryReport summaryReport batchResult
                            case saveResult of
                                Left err -> pure (Left err)
                                Right () -> do
                                    putStrLn $ "Сводный отчёт: " ++ summaryReport
                                    pure (Right ())

deleteRoomMenu :: FilePath -> IO (Either AppError ())
deleteRoomMenu file = do
    roomsResult <- loadRooms file
    case roomsResult of
        Left err -> pure (Left err)
        Right rooms -> do
            if null rooms
            then do
                putStrLn "Сохранённых помещений нет."
                pure (Right ())
            else do
                putStrLn "Сохранённые помещения:"
                forM_ (zip [1..] rooms) $ \(i, r) ->
                    putStrLn $ "  " ++ show i ++ ". " ++ roomName r

                numResult <- readInRange "Номер для удаления: " 1 (length rooms)
                case numResult of
                    Left err -> pure (Left err)
                    Right n -> deleteRoom file n

createRoom :: IO (Either AppError Room)
createRoom = do
    putStrLn ""
    putStrLn "--- СОЗДАНИЕ ПОМЕЩЕНИЯ ---"

    nameResult <- readRoomName
    case nameResult of
        Left err -> pure (Left err)
        Right name -> do
            dimsResult <- readDimensions
            case dimsResult of
                Left err -> pure (Left err)
                Right dims -> do

                    surfacesResult <- readSurfaces dims
                    case surfacesResult of
                        Left err -> pure (Left err)
                        Right surfaces0 -> do

                            let initialRemaining =
                                    [(surfaceId s, surfaceArea s) | s <- surfaces0]

                            windowsResult <- readWindows initialRemaining
                            case windowsResult of
                                Left err -> pure (Left err)
                                Right (windows, afterWindows) -> do

                                    doorsResult <- readDoors afterWindows
                                    case doorsResult of
                                        Left err -> pure (Left err)
                                        Right (doors, afterDoors) -> do

                                            decorResult <- readDecorItems afterDoors
                                            case decorResult of
                                                Left err -> pure (Left err)
                                                Right (decor, _) -> do

                                                    let surfaces =
                                                            attachDecor decor
                                                            $ attachDoors doors
                                                            $ attachWindows windows
                                                            $ surfaces0

                                                    seatsResult <- readSeats dims
                                                    case seatsResult of
                                                        Left err -> pure (Left err)
                                                        Right seats ->
                                                            pure (Right Room
                                                                { roomName       = name
                                                                , roomDimensions = dims
                                                                , roomSurfaces   = surfaces
                                                                , roomSeats      = seats
                                                                })

readRoomName :: IO (Either AppError String)
readRoomName = do
    putStr "Название помещения: "
    hFlush stdout
    name <- getLine
    if null name
    then do
        putStrLn "Название не может быть пустым."
        _ <- logWarning logFile "Пустое название помещения"
        readRoomName
    else pure (Right name)

readDimensions :: IO (Either AppError Dimensions)
readDimensions = do
    lResult <- readPositiveDouble "Длина, м: "
    case lResult of
        Left err -> pure (Left err)
        Right l -> do
            wResult <- readPositiveDouble "Ширина, м: "
            case wResult of
                Left err -> pure (Left err)
                Right w -> do
                    hResult <- readPositiveDouble "Высота, м: "
                    case hResult of
                        Left err -> pure (Left err)
                        Right h -> pure (Right (Dimensions l w h))

readSurfaces :: Dimensions -> IO (Either AppError [Surface])
readSurfaces dims = do
    let h      = roomHeight dims
        wall1A = roomLength dims * h
        wall2A = roomWidth  dims * h
        wall3A = wall1A
        wall4A = wall2A
        floorA = roomLength dims * roomWidth dims
        ceilA  = floorA

    s1 <- readSurface (Wall 1) wall1A
    case s1 of
        Left err -> pure (Left err)
        Right s1' -> do
            s2 <- readSurface (Wall 2) wall2A
            case s2 of
                Left err -> pure (Left err)
                Right s2' -> do
                    s3 <- readSurface (Wall 3) wall3A
                    case s3 of
                        Left err -> pure (Left err)
                        Right s3' -> do
                            s4 <- readSurface (Wall 4) wall4A
                            case s4 of
                                Left err -> pure (Left err)
                                Right s4' -> do
                                    sf <- readSurface Floor floorA
                                    case sf of
                                        Left err -> pure (Left err)
                                        Right sf' -> do
                                            sc <- readSurface Ceiling ceilA
                                            case sc of
                                                Left err -> pure (Left err)
                                                Right sc' -> pure (Right
                                                    [s1', s2', s3', s4', sf', sc'])


readSurface :: SurfaceId -> Double -> IO (Either AppError Surface)
readSurface sid area = do
    putStrLn ""
    putStrLn $ "Поверхность: " ++ show sid
            ++ " (площадь " ++ show area ++ " м²)"

    materialsResult <- loadSurfaceMaterials (materialsDir ++ "/materials.csv")
    case materialsResult of
        Left err -> pure (Left err)
        Right materials -> do
            putStrLn "Доступные материалы:"
            forM_ (zip [1..] materials) $ \(n, m) ->
                putStrLn $ "  " ++ show n ++ ". " ++ surfaceMaterialName m

            numResult <- readInRange "Номер материала: " 1 (length materials)
            case numResult of
                Left err -> pure (Left err)
                Right n ->
                    pure (Right Surface
                        { surfaceId       = sid
                        , surfaceMaterial = materials !! (n - 1)
                        , surfaceArea     = area
                        , surfaceElems    = []
                        })

readWindows :: [(SurfaceId, Double)] -> IO (Either AppError ([(SurfaceId, Window)], [(SurfaceId, Double)]))
readWindows remainingSurfaces = do
    putStrLn ""
    putStr "Есть ли окна? (y/n): "
    hFlush stdout
    answer <- getLine
    case answer of
        "n" -> pure (Right ([], remainingSurfaces))
        "y" -> do
            countResult <- readPositiveInt "Сколько окон? "
            case countResult of
                Left err -> pure (Left err)
                Right n -> do
                    result <- readWindowsLoop remainingSurfaces n []
                    case result of
                        Left err -> pure (Left err)
                        Right (windows, remaining') ->
                            pure (Right (windows, remaining'))
        _ -> do
            putStrLn "Ответьте y или n."
            readWindows remainingSurfaces


readWindowsLoop :: [(SurfaceId, Double)] -> Int -> [(SurfaceId, Window)] -> IO (Either AppError ([(SurfaceId, Window)], [(SurfaceId, Double)]))
readWindowsLoop remainingSurfaces 0 acc =
    pure (Right (reverse acc, remainingSurfaces))
readWindowsLoop remainingSurfaces n acc = do
    putStrLn ""
    putStrLn $ "Окно " ++ show (length acc + 1) ++ ":"

    putStrLn "На какой стене?"
    putStrLn "  1 — стена 1"
    putStrLn "  2 — стена 2"
    putStrLn "  3 — стена 3"
    putStrLn "  4 — стена 4"

    wallResult <- readInRange "Номер: " 1 4
    case wallResult of
        Left err -> pure (Left err)
        Right wallN -> do
            let sid = Wall wallN
                remaining = fromMaybe 0 (lookup sid remainingSurfaces)

            if remaining <= 0
            then do
                putStrLn "На этой стене нет свободной площади."
                readWindowsLoop remainingSurfaces n acc
            else do
                materialsResult <- loadWindowMaterials
                                    (materialsDir ++ "/windows.csv")
                case materialsResult of
                    Left err -> pure (Left err)
                    Right materials -> do
                        putStrLn $ "Доступные материалы для " ++ show sid ++ ":"
                        forM_ (zip [1..] materials) $ \(i, m) ->
                            putStrLn $ "  " ++ show i ++ ". " ++ windowName m

                        numResult <- readInRange "Номер: " 1 (length materials)
                        case numResult of
                            Left err -> pure (Left err)
                            Right i -> do
                                putStrLn $ "Осталось площади: "
                                        ++ show remaining ++ " м²"
                                areaResult <- readPositiveDoubleInRange
                                                "Площадь, м²: "
                                                0
                                                remaining
                                case areaResult of
                                    Left err -> pure (Left err)
                                    Right area -> do
                                        let w = Window
                                                { windowId       =
                                                    "W" ++ show (length acc + 1)
                                                , windowMaterial = materials !! (i - 1)
                                                , windowArea     = area
                                                }
                                            newRemaining =
                                                map (\(sid', r) ->
                                                        if sid' == sid
                                                        then (sid', r - area)
                                                        else (sid', r))
                                                    remainingSurfaces
                                        readWindowsLoop newRemaining
                                                        (n - 1)
                                                        ((sid, w) : acc)

readDoors :: [(SurfaceId, Double)] -> IO (Either AppError ([(SurfaceId, Door)], [(SurfaceId, Double)]))
readDoors remainingSurfaces = do
    putStrLn ""
    putStr "Есть ли двери? (y/n): "
    hFlush stdout
    answer <- getLine
    case answer of
        "n" -> pure (Right ([], remainingSurfaces))
        "y" -> do
            countResult <- readPositiveInt "Сколько дверей? "
            case countResult of
                Left err -> pure (Left err)
                Right n -> do
                    result <- readDoorsLoop remainingSurfaces n []
                    case result of
                        Left err -> pure (Left err)
                        Right (doors, remaining') ->
                            pure (Right (doors, remaining'))
        _ -> do
            putStrLn "Ответьте y или n."
            readDoors remainingSurfaces

readDoorsLoop :: [(SurfaceId, Double)] -> Int -> [(SurfaceId, Door)] -> IO (Either AppError ([(SurfaceId, Door)], [(SurfaceId, Double)]))
readDoorsLoop remainingSurfaces 0 acc =
    pure (Right (reverse acc, remainingSurfaces))
readDoorsLoop remainingSurfaces n acc = do
    putStrLn ""
    putStrLn $ "Дверь " ++ show (length acc + 1) ++ ":"

    putStrLn "На какой стене?"
    putStrLn "  1 — стена 1"
    putStrLn "  2 — стена 2"
    putStrLn "  3 — стена 3"
    putStrLn "  4 — стена 4"

    wallResult <- readInRange "Номер: " 1 4
    case wallResult of
        Left err -> pure (Left err)
        Right wallN -> do
            let sid = Wall wallN
                remaining = fromMaybe 0 (lookup sid remainingSurfaces)

            if remaining <= 0
            then do
                putStrLn "На этой стене нет свободной площади."
                readDoorsLoop remainingSurfaces n acc
            else do
                materialsResult <- loadDoorMaterials
                                    (materialsDir ++ "/doors.csv")
                case materialsResult of
                    Left err -> pure (Left err)
                    Right materials -> do
                        putStrLn $ "Доступные материалы для " ++ show sid ++ ":"
                        forM_ (zip [1..] materials) $ \(i, m) ->
                            putStrLn $ "  " ++ show i ++ ". " ++ doorName m

                        numResult <- readInRange "Номер: " 1 (length materials)
                        case numResult of
                            Left err -> pure (Left err)
                            Right i -> do
                                putStrLn $ "Осталось площади: "
                                        ++ show remaining ++ " м²"
                                areaResult <- readPositiveDoubleInRange
                                                "Площадь, м²: "
                                                0
                                                remaining
                                case areaResult of
                                    Left err -> pure (Left err)
                                    Right area -> do
                                        let d = Door
                                                { doorId       =
                                                    "D" ++ show (length acc + 1)
                                                , doorMaterial = materials !! (i - 1)
                                                , doorArea     = area
                                                }
                                            newRemaining =
                                                map (\(sid', r) ->
                                                        if sid' == sid
                                                        then (sid', r - area)
                                                        else (sid', r))
                                                    remainingSurfaces
                                        readDoorsLoop newRemaining
                                                      (n - 1)
                                                      ((sid, d) : acc)

readDecorItems :: [(SurfaceId, Double)] -> IO (Either AppError ([(SurfaceId, DecorItem)], [(SurfaceId, Double)]))
readDecorItems remainingSurfaces = do
    putStrLn ""
    putStr "Есть ли элементы интерьера (ковры, занавесы)? (y/n): "
    hFlush stdout
    answer <- getLine
    case answer of
        "n" -> pure (Right ([], remainingSurfaces))
        "y" -> do
            countResult <- readPositiveInt "Сколько? "
            case countResult of
                Left err -> pure (Left err)
                Right n -> do
                    result <- readDecorLoop remainingSurfaces n []
                    case result of
                        Left err -> pure (Left err)
                        Right (decor, remaining') ->
                            pure (Right (decor, remaining'))
        _ -> do
            putStrLn "Ответьте y или n."
            readDecorItems remainingSurfaces


readDecorLoop :: [(SurfaceId, Double)] -> Int -> [(SurfaceId, DecorItem)] -> IO (Either AppError ([(SurfaceId, DecorItem)], [(SurfaceId, Double)]))
readDecorLoop remainingSurfaces 0 acc =
    pure (Right (reverse acc, remainingSurfaces))
readDecorLoop remainingSurfaces n acc = do
    putStrLn ""
    putStrLn $ "Элемент " ++ show (length acc + 1) ++ ":"

    putStrLn "На какой поверхности?"
    putStrLn "  1 — стена 1"
    putStrLn "  2 — стена 2"
    putStrLn "  3 — стена 3"
    putStrLn "  4 — стена 4"
    putStrLn "  5 — пол"
    putStrLn "  6 — потолок"

    surfResult <- readInRange "Номер: " 1 6
    case surfResult of
        Left err -> pure (Left err)
        Right n1 -> do
            let sid = case n1 of
                        1 -> Wall 1
                        2 -> Wall 2
                        3 -> Wall 3
                        4 -> Wall 4
                        5 -> Floor
                        _ -> Ceiling

            let remaining = fromMaybe 0 (lookup sid remainingSurfaces)

            if remaining <= 0
            then do
                putStrLn "На этой поверхности нет свободной площади."
                readDecorLoop remainingSurfaces n acc
            else do
                let decorFile = case sid of
                        Floor -> materialsDir ++ "/flour_carpet.csv"
                        _     -> materialsDir ++ "/wall_decor.csv"

                materialsResult <- loadWallDecor decorFile
                case materialsResult of
                    Left err -> pure (Left err)
                    Right materials -> do
                        putStrLn $ "Доступные материалы для " ++ show sid ++ ":"
                        forM_ (zip [1..] materials) $ \(i, m) ->
                            putStrLn $ "  " ++ show i ++ ". " ++ decorName m

                        numResult <- readInRange "Номер: " 1 (length materials)
                        case numResult of
                            Left err -> pure (Left err)
                            Right i -> do
                                putStrLn $ "Осталось площади: "
                                        ++ show remaining ++ " м²"
                                areaResult <- readPositiveDoubleInRange
                                                "Площадь, м²: "
                                                0
                                                remaining
                                case areaResult of
                                    Left err -> pure (Left err)
                                    Right area -> do
                                        let di = DecorItem
                                                { decorMaterial = materials !! (i - 1)
                                                , decorArea     = area
                                                }
                                            newRemaining =
                                                map (\(sid', r) ->
                                                        if sid' == sid
                                                        then (sid', r - area)
                                                        else (sid', r))
                                                    remainingSurfaces
                                        readDecorLoop newRemaining
                                                      (n - 1)
                                                      ((sid, di) : acc)

attachWindows :: [(SurfaceId, Window)] -> [Surface] -> [Surface]
attachWindows items = map addTo
  where
    addTo s =
        let sid = surfaceId s
            matching = [w | (sid', w) <- items, sid' == sid]
            elems = [ElemWindow w | w <- matching]
        in s { surfaceElems = surfaceElems s ++ elems }


attachDoors :: [(SurfaceId, Door)] -> [Surface] -> [Surface]
attachDoors items = map addTo
  where
    addTo s =
        let sid = surfaceId s
            matching = [d | (sid', d) <- items, sid' == sid]
            elems = [ElemDoor d | d <- matching]
        in s { surfaceElems = surfaceElems s ++ elems }


attachDecor :: [(SurfaceId, DecorItem)] -> [Surface] -> [Surface]
attachDecor items = map addTo
  where
    addTo s =
        let sid = surfaceId s
            matching = [d | (sid', d) <- items, sid' == sid]
            elems = [ElemDecor d | d <- matching]
        in s { surfaceElems = surfaceElems s ++ elems }

readSeats :: Dimensions -> IO (Either AppError [SeatItem])
readSeats dims = do
    putStrLn ""
    putStr "Сколько типов сидячих мест? (0 — нет): "
    hFlush stdout
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Нужно число."
            readSeats dims
        Just 0 -> pure (Right [])
        Just n -> readSeatsLoop (calculateFloorArea dims) n []


readSeatsLoop :: Double -> Int -> [SeatItem] -> IO (Either AppError [SeatItem])
readSeatsLoop _ 0 acc = pure (Right (reverse acc))
readSeatsLoop remainingArea n acc = do
    materialsResult <- loadSeatMaterials (materialsDir ++ "/sofa.csv")
    case materialsResult of
        Left err -> pure (Left err)
        Right materials -> do
            putStrLn ""
            putStrLn "Доступные сидячие места:"
            forM_ (zip [1..] materials) $ \(i, m) ->
                putStrLn $ "  " ++ show i ++ ". " ++ seatName m
                        ++ " (" ++ show (seatFloorAreaPerUnit m) ++ " м²/место)"

            numResult <- readInRange "Номер: " 1 (length materials)
            case numResult of
                Left err -> pure (Left err)
                Right i -> do
                    let seatMat = materials !! (i - 1)
                        seatA   = seatFloorAreaPerUnit seatMat
                        maxN    = if seatA <= 0
                                  then 0
                                  else floor (remainingArea / seatA)

                    putStrLn $ "Осталось площади пола: "
                            ++ show remainingArea ++ " м²"
                    putStrLn $ "Максимум мест для этого типа: " ++ show maxN

                    cntResult <- readInRange "Количество: " 0 maxN
                    case cntResult of
                        Left err -> pure (Left err)
                        Right cnt -> do
                            let newRemaining = remainingArea
                                             - fromIntegral cnt * seatA
                                newAcc = SeatItem cnt seatMat : acc
                            readSeatsLoop newRemaining (n - 1) newAcc

readPurpose :: IO (Either AppError PurposeReq)
readPurpose = do
    purposesResult <- loadPurpose (materialsDir ++ "/purpose.csv")
    case purposesResult of
        Left err -> pure (Left err)
        Right purposes -> do
            putStrLn ""
            putStrLn "Доступные назначения:"
            forM_ (zip [1..] purposes) $ \(n, p) ->
                putStrLn $ "  " ++ show n ++ ". " ++ purposeName p

            numResult <- readInRange "Номер назначения: " 1 (length purposes)
            case numResult of
                Left err -> pure (Left err)
                Right n -> pure (Right (purposes !! (n - 1)))

checkRoom :: Room -> PurposeReq -> IO (Either AppError ())
checkRoom room purpose = do
    let volume     = calculateVolume (roomDimensions room)
        commonArea = calculateCommonArea (roomDimensions room) (roomSeats room)
        absorption = calculateTotalAbsorption
                        (roomSurfaces room) (roomSeats room) commonArea
        reverb     = calculateReverberationSpectrum volume absorption commonArea
        acoustic = AcousticResult
            { roomVolume        = volume
            , commonArea        = commonArea
            , totalAbsorption   = absorption
            , reverberationTime = reverb
            }
        suitability = evaluateSuitability acoustic purpose

    putStrLn ""
    putStrLn "=========================================="
    putStrLn "   ИСХОДНЫЙ РЕЗУЛЬТАТ"
    putStrLn "=========================================="
    printAcousticTable acoustic
    putStrLn ""
    printSuitability suitability
    putStrLn ""

    if suitable suitability
    then do
        _ <- logInfo logFile ("Помещение пригодно: " ++ roomName room)
        pure ()
    else do
        _ <- logWarning logFile ("Помещение не пригодно: " ++ roomName room)
        pure ()

    wallResult <- loadWallDecor (materialsDir ++ "/wall_decor.csv")
    case wallResult of
        Left err -> pure (Left err)
        Right wallCatalog -> do
            floorResult <- loadFloorDecor (materialsDir ++ "/flour_carpet.csv")
            case floorResult of
                Left err -> pure (Left err)
                Right floorCatalog -> do
                    let catalog = wallCatalog ++ floorCatalog

                    let reachable = isAchievable room purpose acoustic catalog

                    if not reachable
                    then do
                        putStrLn "=========================================="
                        putStrLn "   РЕКОМЕНДАЦИИ"
                        putStrLn "=========================================="
                        putStrLn "По данной модели норматив не достигается."
                        putStrLn ""

                        let report = Report
                                { reportRoom              = room
                                , reportPurpose           = purpose
                                , reportAcousticBefore    = acoustic
                                , reportSuitabilityBefore = suitability
                                , reportAcousticAfter     = acoustic
                                , reportSuitabilityAfter  = suitability
                                , reportRecommendations   = []
                                }

                        let reportPath = reportDir ++ "/" ++ roomName room ++ ".txt"
                        saveResult <- saveReport reportPath report
                        case saveResult of
                            Left err -> do
                                _ <- logError logFile
                                        ("Ошибка сохранения отчёта: " ++ show err)
                                pure (Left err)
                            Right () -> do
                                putStrLn $ "Отчёт сохранён: " ++ reportPath
                                pure (Right ())

                    else do
                        let recommendations = generateRecommendations
                                                room purpose acoustic suitability
                                                wallCatalog
                                                floorCatalog 
                        putStrLn "=========================================="
                        putStrLn "   РЕКОМЕНДАЦИИ"
                        putStrLn "=========================================="
                        printRecommendations recommendations
                        putStrLn ""

                        putStrLn "=========================================="
                        putStrLn "   ПРОМЕЖУТОЧНЫЕ РЕЗУЛЬТАТЫ"
                        putStrLn "=========================================="
                        printIntermediateSteps recommendations
                        putStrLn ""

                        let newRoom        = applyRecommendations recommendations room
                            newAcoustic    = recalculateAcoustic newRoom
                            newSuitability = evaluateSuitability newAcoustic purpose

                        putStrLn "=========================================="
                        putStrLn "   ИТОГОВЫЙ РЕЗУЛЬТАТ"
                        putStrLn "=========================================="
                        printAcousticTable newAcoustic
                        putStrLn ""
                        printSuitability newSuitability
                        putStrLn ""

                        let report = Report
                                { reportRoom              = room
                                , reportPurpose           = purpose
                                , reportAcousticBefore    = acoustic
                                , reportSuitabilityBefore = suitability
                                , reportAcousticAfter     = newAcoustic
                                , reportSuitabilityAfter  = newSuitability
                                , reportRecommendations   = recommendations
                                }

                        let reportPath = reportDir ++ "/" ++ roomName room ++ ".txt"
                        saveResult <- saveReport reportPath report
                        case saveResult of
                            Left err -> do
                                _ <- logError logFile
                                        ("Ошибка сохранения отчёта: " ++ show err)
                                pure (Left err)
                            Right () -> do
                                putStrLn $ "Отчёт сохранён: " ++ reportPath
                                pure (Right ())


readInt :: String -> IO (Either AppError Int)
readInt prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Нужно целое число."
            _ <- logWarning logFile
                    ("Некорректный ввод целого числа: " ++ input)
            readInt prompt
        Just n -> pure (Right n)


readPositiveInt :: String -> IO (Either AppError Int)
readPositiveInt prompt = do
    result <- readInt prompt
    case result of
        Left err -> pure (Left err)
        Right n
            | n > 0     -> pure (Right n)
            | otherwise -> do
                putStrLn "Число должно быть > 0."
                _ <- logWarning logFile
                        ("Некорректное положительное число: "
                         ++ show n) 
                readPositiveInt prompt


readDouble :: String -> IO (Either AppError Double)
readDouble prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Нужно число."
            _ <- logWarning logFile
                    ("Некорректный ввод числа: " ++ input)
            readDouble prompt
        Just x -> pure (Right x)


readPositiveDouble :: String -> IO (Either AppError Double)
readPositiveDouble prompt = do
    result <- readDouble prompt
    case result of
        Left err -> pure (Left err)
        Right x
            | x > 0     -> pure (Right x)
            | otherwise -> do
                putStrLn "Число должно быть > 0."
                _ <- logWarning logFile
                        ("Некорректное положительное число: "
                         ++ show x)   
                readPositiveDouble prompt


readPositiveDoubleInRange :: String -> Double -> Double -> IO (Either AppError Double)
readPositiveDoubleInRange prompt lo hi = do
    result <- readDouble prompt
    case result of
        Left err -> pure (Left err)
        Right x
            | x <= lo   -> do
                putStrLn $ "Число должно быть > " ++ show lo
                _ <- logWarning logFile
                        ("Число вне диапазона: " ++ show x
                         ++ " (ожидалось > " ++ show lo ++ ")")
                readPositiveDoubleInRange prompt lo hi
            | x > hi    -> do
                putStrLn $ "Число должно быть ≤ " ++ show hi
                _ <- logWarning logFile
                        ("Число вне диапазона: " ++ show x
                         ++ " (ожидалось ≤ " ++ show hi ++ ")") 
                readPositiveDoubleInRange prompt lo hi
            | otherwise -> pure (Right x)


readInRange :: String -> Int -> Int -> IO (Either AppError Int)
readInRange prompt lo hi = do
    result <- readInt prompt
    case result of
        Left err -> pure (Left err)
        Right n
            | n >= lo && n <= hi -> pure (Right n)
            | otherwise -> do
                putStrLn $ "Число от " ++ show lo ++ " до " ++ show hi
                _ <- logWarning logFile
                        ("Число вне диапазона: " ++ show n
                         ++ " (ожидалось от " ++ show lo
                         ++ " до " ++ show hi ++ ")")
                readInRange prompt lo hi


printAcousticTable :: AcousticResult -> IO ()
printAcousticTable acoustic = do
    putStrLn "Частота  | A, м²·сэбин | T, с"
    putStrLn "---------|-------------|------"
    printRow "125 Гц"  (at125  a) (at125  t)
    printRow "500 Гц"  (at500  a) (at500  t)
    printRow "2000 Гц" (at2000 a) (at2000 t)
  where
    a = totalAbsorption acoustic
    t = reverberationTime acoustic
    printRow f av tv =
        putStrLn $ f ++ " | " ++ show av ++ " | " ++ show tv


printSuitability :: SuitabilityResult -> IO ()
printSuitability suitability = do
    putStrLn $ "Относительное отклонение: " ++ show (relativeDev suitability)
    putStrLn $ "Оценка: " ++ gradeText (grade suitability)


gradeText :: Grade -> String
gradeText Suitable              = "ПРИГОДНО"
gradeText ConditionallySuitable = "УСЛОВНО ПРИГОДНО"
gradeText Unsuitable            = "НЕ ПРИГОДНО"


printRecommendations :: [Recommendation] -> IO ()
printRecommendations [] = putStrLn "Рекомендации не требуются."
printRecommendations recs = do
    forM_ (zip [1..] recs) $ \(n, r) -> do
        putStrLn $ show n ++ ". " ++ actionText (action r)
            ++ " " ++ decorName (targetElement r)
        putStrLn $ "   Поверхность: " ++ show (targetSurface r)
        putStrLn $ "   Площадь: " ++ show (recommendedArea r) ++ " м²"


actionText :: Action -> String
actionText Add    = "Добавить"
actionText Remove = "Убрать"

handleResult :: Either AppError () -> IO ()
handleResult (Left err)  = printError err
handleResult (Right ())  = pure ()


printError :: AppError -> IO ()
printError err = do
    putStrLn ""
    putStrLn $ "Ошибка: " ++ show err
    _ <- logError logFile (show err)
    pure ()

printIntermediateSteps :: [Recommendation] -> IO ()
printIntermediateSteps [] = putStrLn "Промежуточных шагов нет."
printIntermediateSteps recs = do
    forM_ (zip [1..] recs) $ \(n, r) -> do
        putStrLn $ "Шаг " ++ show n ++ ": "
                ++ actionText (action r) ++ " "
                ++ decorName (targetElement r)
                ++ " на " ++ show (targetSurface r)
                ++ " (" ++ show (recommendedArea r) ++ " м²)"
        putStrLn $ "  RT60 до:    " ++ formatSpectrum (previousReverb r)
        putStrLn $ "  RT60 после: " ++ formatSpectrum (newReverb r)
        putStrLn ""

formatSpectrum :: Spectrum -> String
formatSpectrum s =
    show (at125 s) ++ " / " ++
    show (at500 s) ++ " / " ++
    show (at2000 s) ++ " с"