{-# LANGUAGE OverloadedStrings #-}

module Storage.JSON
    ( loadRooms
    , saveRooms
    , addRoom
    , showRooms
    , deleteRoom
    , chooseExistingFileInDir
    , chooseOrCreateFileInDir
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Aeson (encode, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.List (findIndex)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

import Types.AppError
    ( AppError(..)
    )
import Types.Room

loadRooms :: FilePath -> IO (Either AppError [Room])
loadRooms filePath = do
    exists <- doesFileExist filePath
    if not exists
    then do
        let emptyContent = encodePretty ([] :: [Room])
        writeResult <- try (BL.writeFile filePath emptyContent)
            :: IO (Either IOException ())
        case writeResult of
            Left err -> pure (Left (FileError (show err)))
            Right () -> pure (Right [])
    else do
        readResult <- try (BL.readFile filePath)
            :: IO (Either IOException BL.ByteString)
        case readResult of
            Left err -> pure (Left (FileError (show err)))
            Right content ->
                if BL.null (BL.filter (not . isSpace') content)
                then pure (Right [])
                else case decode content of
                    Nothing -> pure (Left (ParseError "Не удалось прочитать JSON"))
                    Just rooms -> pure (Right rooms)
  where
    isSpace' w = w == 32 || w == 9 || w == 10 || w == 13

saveRooms :: FilePath -> [Room] -> IO (Either AppError ())
saveRooms filePath rooms = do
    let content = encodePretty rooms
    result <- try (BL.writeFile filePath content)
        :: IO (Either IOException ())
    case result of
        Left err -> pure (Left (FileError (show err)))
        Right () -> pure (Right ())

addRoom :: FilePath -> Room -> Bool -> IO (Either AppError ())
addRoom filePath room overwrite = do
    roomsResult <- loadRooms filePath
    case roomsResult of
        Left err -> pure (Left err)
        Right rooms ->
            case findIndex (\r -> roomName r == roomName room) rooms of
                Just idx
                    | overwrite ->
                        saveRooms filePath (replaceAt idx room rooms)
                    | otherwise ->
                        pure (Left (InvalidInput
                            ("Помещение с именем \"" ++ roomName room
                             ++ "\" уже существует.")))
                Nothing ->
                    saveRooms filePath (rooms ++ [room])

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

showRooms :: FilePath -> IO (Either AppError ())
showRooms filePath = do
    result <- loadRooms filePath
    case result of
        Left err -> pure (Left err)
        Right rooms ->
            if null rooms
            then do
                putStrLn "Сохранённых помещений нет."
                pure (Right ())
            else do
                putStrLn "Сохранённые помещения:"
                forM_ (zip [1..] rooms) $ \(n, room) ->
                    putStrLn $ "  " ++ show n ++ ". " ++ roomName room
                pure (Right ())

deleteRoom :: FilePath -> Int -> IO (Either AppError ())
deleteRoom filePath number = do
    result <- loadRooms filePath
    case result of
        Left err -> pure (Left err)
        Right rooms ->
            if number < 1 || number > length rooms
            then pure (Left (NotFound "Помещение с таким номером не найдено"))
            else do
                let newRooms = take (number - 1) rooms ++ drop number rooms
                saveResult <- saveRooms filePath newRooms
                case saveResult of
                    Left err -> pure (Left err)
                    Right () -> do
                        putStrLn "Помещение удалено."
                        pure (Right ())


chooseOrCreateFileInDir :: FilePath -> IO (Either AppError FilePath)
chooseOrCreateFileInDir dir = do
    putStrLn ""
    putStrLn "1. Использовать существующий файл"
    putStrLn "2. Создать новый файл"
    putStrLn "0. Отмена"
    putStr "Выбор: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            result <- chooseExistingFileInDir dir
            case result of
                Left (InvalidInput _) -> do
                    chooseOrCreateFileInDir dir
                other -> pure other
        "2" -> do
            result <- createNewFileInDir dir
            case result of
                Left (InvalidInput _) -> do
                    chooseOrCreateFileInDir dir
                other -> pure other
        "0" -> pure (Left (InvalidInput "Операция отменена"))
        _   -> do
            putStrLn "Некорректный выбор."
            chooseOrCreateFileInDir dir


askYesNo :: String -> IO Bool
askYesNo prompt = do
    putStrLn prompt
    answer <- getLine
    case answer of
        "y" -> pure True
        "n" -> pure False
        _   -> do
            putStrLn "Ответьте y или n."
            askYesNo prompt


chooseExistingFileInDir :: FilePath -> IO (Either AppError FilePath)
chooseExistingFileInDir dir = do
    putStr "Имя файла (без пути и .json, 0 — отмена): "
    hFlush stdout
    name <- getLine
    if null name
    then do
        putStrLn "Имя не может быть пустым."
        chooseExistingFileInDir dir
    else if name == "0"
         then pure (Left (InvalidInput "Отмена"))
         else do
            let path = dir ++ "/" ++ name ++ ".json"
            exists <- doesFileExist path
            if exists
            then pure (Right path)
            else do
                putStrLn $ "Файл не найден: " ++ path
                retry <- askYesNo "Попробовать снова? (y/n)"
                if retry
                then chooseExistingFileInDir dir
                else pure (Left (InvalidInput "Отмена"))


createNewFileInDir :: FilePath -> IO (Either AppError FilePath)
createNewFileInDir dir = do
    putStr "Имя нового файла (без пути и .json, 0 — отмена): "
    hFlush stdout
    name <- getLine
    if null name
    then do
        putStrLn "Имя не может быть пустым."
        createNewFileInDir dir
    else if name == "0"
         then pure (Left (InvalidInput "Отмена"))
         else do
            let path = dir ++ "/" ++ name ++ ".json"
            exists <- doesFileExist path
            if exists
            then do
                putStrLn $ "Файл уже существует: " ++ path
                putStrLn "Перезаписать? (y/n)"
                answer <- getLine
                case answer of
                    "y" -> do
                        result <- saveRooms path []
                        case result of
                            Left err -> pure (Left err)
                            Right () -> pure (Right path)
                    "n" -> pure (Left (InvalidInput "Отмена"))
                    _   -> do
                        putStrLn "Ответьте y или n."
                        createNewFileInDir dir
            else do
                result <- saveRooms path []
                case result of
                    Left err -> pure (Left err)
                    Right () -> do
                        putStrLn $ "Файл создан: " ++ path
                        pure (Right path)