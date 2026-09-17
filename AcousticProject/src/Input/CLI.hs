module Input.CLI
    ( mainMenu
    , createRoom
    , readRoomName
    , readDimensions
    , readSurface
    , readSurfaces
    , readAdjustableItem
    , readAdjustableItems
    , checkRoom
    , readEvent
    ) where

import Types.AppError
import Types.Geometry
import Types.Material
    ( Surface(..)
    )
import qualified Types.Material as Material
    ( materialName
    )
import Types.Room
import Types.Event
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation
import Types.Report
import Text.Read
    ( readMaybe
    )
import System.IO
    ( hFlush
    , stdout
    )

import Input.CSV
    ( showMaterials
    , showRemovableMaterials
    , getMaterialByNumber
    , getRemovableMaterialByNumber
    , showEventRequirements
    , getEventRequirementByNumber
    , loadMaterials
    , loadEvents
    )

import Storage.JSON
    ( loadRooms
    , addRoom
    , showRooms
    , getRoomByNumber
    )

import Calculation.Geometry
    ( calculateVolume
    )

import Calculation.Absorption
    ( calculateAbsorptionArea
    )

import Calculation.Reverberation
    ( calculateReverberationTime
    )

import Evaluation.Suitability
    ( evaluateSuitability
    )

import Recommendation.Acoustic
    ( generateRecommendations
    )

import Output.Report
    ( saveReport
    )

import Log.Logger
    ( logInfo
    , logWarning
    , logError
    )


roomsFile :: FilePath
roomsFile = "data/rooms.json"


reportFile :: FilePath
reportFile = "data/report.txt"

logFile :: FilePath
logFile = "data/app.log"


mainMenu :: FilePath -> FilePath -> IO (Either AppError ())
mainMenu materialsPath eventsPath = do
    putStrLn ""
    putStrLn "======================================"
    putStrLn "     АКУСТИЧЕСКИЙ АНАЛИЗ ПОМЕЩЕНИЯ"
    putStrLn "======================================"
    putStrLn "1. Создать помещение"
    putStrLn "2. Просмотреть сохранённые помещения"
    putStrLn "3. Проверить помещение"
    putStrLn "0. Выход"
    putStrLn "======================================"
    putStr "Выберите действие: "
    hFlush stdout

    input <- getLine

    case input of
        "1" -> do
            result <- createRoom materialsPath

            case result of
                Left err -> do
                    _ <-
                        logError
                            logFile
                            ("Ошибка создания помещения: "
                                ++ show err)

                    printError err
                    pure (Left err)

                Right room -> do
                    saveResult <- addRoom roomsFile room

                    case saveResult of
                        Left err -> do
                            _ <-
                                logError
                                    logFile
                                    ("Ошибка сохранения помещения: "
                                        ++ show err)

                            printError err
                            pure (Left err)

                        Right () -> do
                            putStrLn ""
                            putStrLn "Помещение успешно сохранено."

                            logResult <-
                                logInfo
                                    logFile
                                    ("Помещение сохранено: " ++ roomName room)

                            case logResult of
                                Left err -> do
                                    printError err
                                    pure (Left err)

                                Right () ->
                                    mainMenu materialsPath eventsPath

        "2" -> do
            result <- showRooms roomsFile

            case result of
                Left err -> do
                    _ <-
                        logError
                            logFile
                            ("Ошибка чтения помещений: "
                                ++ show err)

                    printError err
                    pure (Left err)

                Right () ->
                    mainMenu materialsPath eventsPath

        "3" -> do
            result <- checkRoom eventsPath

            case result of
                Left err -> do
                    _ <-
                        logError
                            logFile
                            ("Ошибка проверки помещения: "
                                ++ show err)

                    printError err
                    pure (Left err)

                Right () ->
                    mainMenu materialsPath eventsPath

        "0" ->
            pure (Right ())

        _ -> do
            putStrLn ""
            putStrLn "Ошибка: некорректный пункт меню."

            _ <-
                logWarning
                    logFile
                    ("Некорректный пункт меню: " ++ input)
            mainMenu materialsPath eventsPath


createRoom
    :: FilePath
    -> IO (Either AppError Room)
createRoom materialsPath = do

    putStrLn ""
    putStrLn "======================================"
    putStrLn "          СОЗДАНИЕ ПОМЕЩЕНИЯ"
    putStrLn "======================================"

    nameResult <-
        readRoomName

    case nameResult of
        Left err ->
            pure (Left err)

        Right name -> do

            dimensionsResult <-
                readDimensions

            case dimensionsResult of
                Left err ->
                    pure (Left err)

                Right roomDimensions -> do

                    surfacesResult <-
                        readSurfaces
                            materialsPath
                            roomDimensions

                    case surfacesResult of
                        Left err ->
                            pure (Left err)

                        Right roomSurfaces -> do

                            adjustableResult <-
                                readAdjustableItems
                                    materialsPath

                            case adjustableResult of
                                Left err ->
                                    pure (Left err)

                                Right items ->
                                    pure
                                        (Right
                                            Room
                                                { roomName =
                                                    name
                                                , dimensions =
                                                    roomDimensions
                                                , surfaces =
                                                    roomSurfaces
                                                , adjustableItems =
                                                    items
                                                })


readRoomName :: IO (Either AppError String)
readRoomName = do
    putStr "Введите название помещения: "
    hFlush stdout

    name <- getLine

    if null name
    then do
        putStrLn
            "Ошибка: название помещения не может быть пустым."
        readRoomName
    else
        pure (Right name)


readDimensions
    :: IO (Either AppError Dimensions)
readDimensions = do

    putStrLn ""
    putStrLn "Введите размеры помещения."

    lengthResult <-
        readPositiveDouble
            "Длина, м: "

    case lengthResult of
        Left err ->
            pure (Left err)

        Right roomLengthValue -> do

            widthResult <-
                readPositiveDouble
                    "Ширина, м: "

            case widthResult of
                Left err ->
                    pure (Left err)

                Right roomWidthValue -> do

                    heightResult <-
                        readPositiveDouble
                            "Высота, м: "

                    case heightResult of
                        Left err ->
                            pure (Left err)

                        Right roomHeightValue ->
                            pure
                                (Right
                                    Dimensions
                                        { roomLength =
                                            roomLengthValue
                                        , roomWidth =
                                            roomWidthValue
                                        , roomHeight =
                                            roomHeightValue
                                        })


readSurface :: FilePath -> Double -> IO (Either AppError Surface)
readSurface materialsPath area = do
    putStrLn $
        "Площадь поверхности: "
            ++ show area
            ++ " м²"

    showResult <- showMaterials materialsPath

    case showResult of
        Left err ->
            pure (Left err)

        Right () -> do
            materialResult <- loadMaterials materialsPath

            case materialResult of
                Left err ->
                    pure (Left err)

                Right materials -> do
                    numberResult <-
                        readIntInRange
                            "Номер материала: "
                            1
                            (length materials)

                    case numberResult of
                        Left err ->
                            pure (Left err)

                        Right number -> do
                            materialResult <-
                                getMaterialByNumber
                                    materialsPath
                                    number

                            case materialResult of
                                Left err ->
                                    pure (Left err)

                                Right material ->
                                    pure
                                        (Right
                                            Surface
                                                { surfaceMaterial =
                                                    material
                                                , surfaceArea =
                                                    area
                                                })


readSurfaces
    :: FilePath
    -> Dimensions
    -> IO (Either AppError [Surface])
readSurfaces materialsPath dimensions = do

    putStrLn ""
    putStrLn "======================================"
    putStrLn "          ПОВЕРХНОСТИ ПОМЕЩЕНИЯ"
    putStrLn "======================================"

    putStrLn ""
    putStrLn "Стена 1:"
    wall1 <-
        readSurface
            materialsPath
            (roomLength dimensions
                * roomHeight dimensions)

    case wall1 of
        Left err ->
            pure (Left err)

        Right surface1 -> do

            putStrLn ""
            putStrLn "Стена 2:"
            wall2 <-
                readSurface
                    materialsPath
                    (roomWidth dimensions
                        * roomHeight dimensions)

            case wall2 of
                Left err ->
                    pure (Left err)

                Right surface2 -> do

                    putStrLn ""
                    putStrLn "Стена 3:"
                    wall3 <-
                        readSurface
                            materialsPath
                            (roomLength dimensions
                                * roomHeight dimensions)

                    case wall3 of
                        Left err ->
                            pure (Left err)

                        Right surface3 -> do

                            putStrLn ""
                            putStrLn "Стена 4:"
                            wall4 <-
                                readSurface
                                    materialsPath
                                    (roomWidth dimensions
                                        * roomHeight dimensions)

                            case wall4 of
                                Left err ->
                                    pure (Left err)

                                Right surface4 -> do

                                    putStrLn ""
                                    putStrLn "Пол:"
                                    floorSurface <-
                                        readSurface
                                            materialsPath
                                            (roomLength dimensions
                                                * roomWidth dimensions)

                                    case floorSurface of
                                        Left err ->
                                            pure (Left err)

                                        Right floor -> do

                                            putStrLn ""
                                            putStrLn "Потолок:"
                                            ceilingSurface <-
                                                readSurface
                                                    materialsPath
                                                    (roomLength dimensions
                                                        * roomWidth dimensions)

                                            case ceilingSurface of
                                                Left err ->
                                                    pure (Left err)

                                                Right ceiling ->
                                                    pure
                                                        (Right
                                                            [ surface1
                                                            , surface2
                                                            , surface3
                                                            , surface4
                                                            , floor
                                                            , ceiling
                                                            ])


readAdjustableItem :: FilePath -> IO (Either AppError AdjustableItem)
readAdjustableItem materialsPath = do
    showResult <- showRemovableMaterials materialsPath

    case showResult of
        Left err ->
            pure (Left err)

        Right removableMaterials -> do
            let count = length removableMaterials

            numberResult <-
                readIntInRange
                    "Номер материала: "
                    1
                    count

            case numberResult of
                Left err ->
                    pure (Left err)

                Right number -> do
                    materialResult <-
                        getRemovableMaterialByNumber
                            materialsPath
                            number

                    case materialResult of
                        Left err ->
                            pure (Left err)

                        Right material -> do
                            areaResult <-
                                readPositiveDouble
                                    "Площадь элемента, м²: "

                            case areaResult of
                                Left err ->
                                    pure (Left err)

                                Right area ->
                                    pure
                                        (Right
                                            AdjustableItem
                                                { itemName =
                                                    Material.materialName
                                                        material
                                                , itemMaterial =
                                                    material
                                                , itemArea =
                                                    area
                                                })

readAdjustableItems
    :: FilePath
    -> IO (Either AppError [AdjustableItem])
readAdjustableItems materialsPath = do

    countResult <-
        readNonNegativeInt
            "Количество регулируемых элементов: "

    case countResult of
        Left err ->
            pure (Left err)

        Right count ->
            readAdjustableItemsLoop
                materialsPath
                count
                []


readAdjustableItemsLoop
    :: FilePath
    -> Int
    -> [AdjustableItem]
    -> IO (Either AppError [AdjustableItem])
readAdjustableItemsLoop
    materialsPath
    count
    items

    | count == 0 =
        pure (Right (reverse items))

    | otherwise = do

        putStrLn ""
        putStrLn $
            "Регулируемый элемент "
                ++ show
                    (length items + 1)
                ++ " из "
                ++ show
                    (length items + count)

        result <-
            readAdjustableItem
                materialsPath

        case result of
            Left err ->
                pure (Left err)

            Right item ->
                readAdjustableItemsLoop
                    materialsPath
                    (count - 1)
                    (item : items)


readEvent :: FilePath -> IO (Either AppError EventRequirement)
readEvent eventsPath = do
    putStrLn ""

    showResult <- showEventRequirements eventsPath

    case showResult of
        Left err ->
            pure (Left err)

        Right () -> do
            eventsResult <- loadEvents eventsPath

            case eventsResult of
                Left err ->
                    pure (Left err)

                Right events -> do
                    numberResult <-
                        readIntInRange
                            "Номер мероприятия: "
                            1
                            (length events)

                    case numberResult of
                        Left err ->
                            pure (Left err)

                        Right number ->
                            getEventRequirementByNumber
                                eventsPath
                                number


checkRoom :: FilePath -> IO (Either AppError ())
checkRoom eventsPath = do
    putStrLn ""
    putStrLn "======================================"
    putStrLn "           ПРОВЕРКА ПОМЕЩЕНИЯ"
    putStrLn "======================================"

    roomsResult <- loadRooms roomsFile

    case roomsResult of
        Left err ->
            pure (Left err)

        Right rooms ->
            if null rooms
            then do
                putStrLn "Сохранённых помещений нет."
                pure (Right ())

            else do
                showRoomsResult <- showRooms roomsFile

                case showRoomsResult of
                    Left err ->
                        pure (Left err)

                    Right () -> do
                        numberResult <-
                            readIntInRange
                                "Номер помещения: "
                                1
                                (length rooms)

                        case numberResult of
                            Left err ->
                                pure (Left err)

                            Right number -> do
                                roomResult <-
                                    getRoomByNumber
                                        roomsFile
                                        number

                                case roomResult of
                                    Left err ->
                                        pure (Left err)

                                    Right room -> do
                                        putStrLn ""
                                        putStrLn $
                                            "Помещение: "
                                                ++ roomName room

                                        eventResult <-
                                            readEvent eventsPath

                                        case eventResult of
                                            Left err ->
                                                pure (Left err)

                                            Right event -> do
                                                putStrLn ""
                                                putStrLn $
                                                    "Мероприятие: "
                                                        ++ eventName
                                                            (eventType event)

                                                let volume =
                                                        calculateVolume
                                                            (dimensions room)

                                                let absorption =
                                                        calculateAbsorptionArea
                                                            room

                                                let reverb =
                                                        calculateReverberationTime
                                                            volume
                                                            absorption

                                                let acoustic =
                                                        AcousticResult
                                                            { roomVolume =
                                                                volume
                                                            , absorptionArea =
                                                                absorption
                                                            , reverberationTime =
                                                                reverb
                                                            }

                                                let suitability =
                                                        evaluateSuitability
                                                            acoustic
                                                            event

                                                let recommendations =
                                                        generateRecommendations
                                                            room
                                                            event
                                                            acoustic
                                                            suitability

                                                let report =
                                                        Report
                                                            { reportRoom =
                                                                room
                                                            , reportEvent =
                                                                event
                                                            , reportAcoustic =
                                                                acoustic
                                                            , reportSuitability =
                                                                suitability
                                                            , reportRecommendations =
                                                                recommendations
                                                            }

                                                putStrLn ""
                                                putStrLn "=== РЕЗУЛЬТАТ РАСЧЁТА ==="

                                                putStrLn $
                                                    "Объём: "
                                                        ++ show volume
                                                        ++ " м³"

                                                putStrLn $
                                                    "Эквивалентная площадь звукопоглощения: "
                                                        ++ show absorption
                                                        ++ " м²"

                                                putStrLn $
                                                    "Время реверберации: "
                                                        ++ show reverb
                                                        ++ " с"

                                                putStrLn $
                                                    "Норма RT60: "
                                                        ++ show
                                                            (fst
                                                                (recommendedReverb event))
                                                        ++ " - "
                                                        ++ show
                                                            (snd
                                                                (recommendedReverb event))
                                                        ++ " с"

                                                putStrLn ""

                                                if suitable suitability
                                                then do
                                                    _ <-
                                                        logInfo
                                                            logFile
                                                            ("Помещение соответствует требованиям: "
                                                                ++ roomName room)
                                                    pure ()
                                                else do
                                                    _ <-
                                                        logWarning
                                                            logFile
                                                            ("Помещение не соответствует требованиям: "
                                                                ++ roomName room)
                                                    pure ()
                                                putStrLn ""
                                                putStrLn "=== РЕКОМЕНДАЦИИ ==="

                                                if null recommendations
                                                then
                                                    putStrLn
                                                        "Рекомендации отсутствуют."
                                                else
                                                    mapM_
                                                        printRecommendation
                                                        (zip
                                                            [1..]
                                                            recommendations)

                                                reportResult <-
                                                    saveReport
                                                        reportFile
                                                        report

                                                case reportResult of
                                                    Left err -> do
                                                        _ <-
                                                            logError
                                                                logFile
                                                                ("Ошибка сохранения отчёта: "
                                                                    ++ show err)

                                                        pure (Left err)

                                                    Right () -> do
                                                        putStrLn ""
                                                        putStrLn $
                                                            "Отчёт сохранён в: "
                                                                ++ reportFile
                                                        _ <-
                                                            logInfo
                                                                logFile
                                                                ("Отчёт сохранён для помещения: "
                                                                    ++ roomName room)
                                                        pure (Right ())


printRecommendation
    :: (Int, Recommendation)
    -> IO ()
printRecommendation
    (number, recommendation) = do

    putStrLn $
        show number
            ++ ". "
            ++ action recommendation

    putStrLn $
        "   Материал: "
            ++ materialName recommendation

    putStrLn $
        "   RT60: "
            ++ show
                (previousReverb recommendation)
            ++ " -> "
            ++ show
                (newReverb recommendation)
            ++ " с"

    putStrLn $
        "   "
            ++ recommendationResult
                (result recommendation)


recommendationResult
    :: SuitabilityResult
    -> String
recommendationResult suitability

    | suitable suitability =
        "После изменения помещение соответствует нормативу."

    | otherwise =
        "После изменения помещение всё ещё не соответствует нормативу."


readInt :: String -> IO (Either AppError Int)
readInt message = do
    putStr message
    hFlush stdout

    input <- getLine

    case readMaybe input of
        Nothing -> do
            putStrLn "Ошибка: нужно ввести целое число."

            _ <-
                logWarning
                        logFile
                        ("Некорректный ввод целого числа: " ++ input)

            readInt message

        Just value ->
            pure (Right value)


readPositiveInt :: String -> IO (Either AppError Int)
readPositiveInt message = do
    result <- readInt message

    case result of
        Left err ->
            pure (Left err)

        Right value ->
            if value > 0
            then
                pure (Right value)
            else do
                putStrLn "Ошибка: число должно быть больше нуля."

                _ <-
                    logWarning
                            logFile
                            ( "Некорректное положительное число: "
                                ++ show value
                            )

                readPositiveInt message


readNonNegativeInt :: String -> IO (Either AppError Int)
readNonNegativeInt message = do
    result <- readInt message

    case result of
        Left err ->
            pure (Left err)

        Right value ->
            if value >= 0
            then
                pure (Right value)
            else do
                putStrLn "Ошибка: число не может быть отрицательным."

                _ <-
                    logWarning
                        logFile
                        ( "Некорректное положительное число: "
                            ++ show value
                        )

                readNonNegativeInt message


readDouble :: String -> IO (Either AppError Double)
readDouble message = do
    putStr message
    hFlush stdout

    input <- getLine

    case readMaybe input of
        Nothing -> do
            putStrLn "Ошибка: нужно ввести число."

            _ <-
                logWarning
                    logFile
                    ("Некорректный ввод числа: " ++ input)

            readDouble message

        Just value ->
            pure (Right value)


readPositiveDouble :: String -> IO (Either AppError Double)
readPositiveDouble message = do
    result <- readDouble message

    case result of
        Left err ->
            pure (Left err)

        Right value ->
            if value > 0
            then
                pure (Right value)
            else do
                putStrLn "Ошибка: число должно быть больше нуля."

                _ <-
                    logWarning
                            logFile
                            ( "Некорректное положительное число: "
                                ++ show value
                            )

                readPositiveDouble message

readIntInRange
    :: String
    -> Int
    -> Int
    -> IO (Either AppError Int)
readIntInRange message minValue maxValue = do
    result <- readInt message

    case result of
        Left err ->
            pure (Left err)

        Right value ->
            if value >= minValue && value <= maxValue
            then
                pure (Right value)
            else do
                putStrLn $
                    "Ошибка: введите число от "
                        ++ show minValue
                        ++ " до "
                        ++ show maxValue
                        ++ "."
                _ <-
                    logWarning
                        logFile
                        ( "Число вне допустимого диапазона: "
                            ++ show value
                            ++ " (ожидалось от "
                            ++ show minValue
                            ++ " до "
                            ++ show maxValue
                            ++ ")"
                        )
                readIntInRange
                    message
                    minValue
                    maxValue

printError
    :: AppError
    -> IO ()
printError err = do

    putStrLn ""
    putStrLn $
        "Ошибка: "
            ++ show err