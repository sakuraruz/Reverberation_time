{-# LANGUAGE OverloadedStrings #-}

module Storage.JSON
    ( loadRooms
    , addRoom
    , showRooms
    , getRoomByNumber
    ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
    ( FromJSON
    , ToJSON
    , decode
    , encode
    )

import Types.AppError
import Types.Room


loadRooms :: FilePath -> IO (Either AppError [Room])
loadRooms filePath = do
    result <- try (BL.readFile filePath)
        :: IO (Either IOException BL.ByteString)

    case result of
        Left _ -> do
            let emptyRooms = encode ([] :: [Room])

            writeResult <- try (BL.writeFile filePath emptyRooms)
                :: IO (Either IOException ())

            case writeResult of
                Left err ->
                    pure (Left (FileError (show err)))

                Right () ->
                    pure (Right [])

        Right content ->
            if BL.null content
                then do
                    BL.writeFile filePath (encode ([] :: [Room]))
                    pure (Right [])
                else
                    case decode content of
                        Nothing ->
                            pure (Left
                                (ParseError
                                    "Не удалось прочитать JSON-файл"))

                        Just rooms ->
                            pure (Right rooms)


addRoom :: FilePath -> Room -> IO (Either AppError ())
addRoom filePath room = do
    result <- loadRooms filePath

    case result of
        Left err ->
            pure (Left err)

        Right rooms -> do
            BL.writeFile filePath (encode (rooms ++ [room]))
            pure (Right ())


showRooms :: FilePath -> IO (Either AppError ())
showRooms filePath = do
    result <- loadRooms filePath

    case result of
        Left err ->
            pure (Left err)

        Right rooms -> do
            if null rooms
                then putStrLn "Сохранённых помещений нет."
                else do
                    putStrLn "Сохранённые помещения:"
                    mapM_ printRoom (zip [1..] rooms)

            pure (Right ())


printRoom :: (Int, Room) -> IO ()
printRoom (number, room) =
    putStrLn $
        show number
        ++ ". "
        ++ roomName room


getRoomByNumber :: FilePath -> Int -> IO (Either AppError Room)
getRoomByNumber filePath number = do
    result <- loadRooms filePath

    case result of
        Left err ->
            pure (Left err)

        Right rooms ->
            case lookup number (zip [1..] rooms) of
                Nothing ->
                    pure (Left
                        (NotFound
                            "Помещение с таким номером не найдено"))

                Just room ->
                    pure (Right room)