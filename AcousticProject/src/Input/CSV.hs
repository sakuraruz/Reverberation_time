{-# LANGUAGE OverloadedStrings #-}

module Input.CSV
    ( showMaterials
    , showRemovableMaterials
    , getMaterialByNumber
    , getRemovableMaterialByNumber
    , showEventRequirements
    , getEventRequirementByNumber
    , loadMaterials
    , loadEvents
    ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
    ( FromNamedRecord(..)
    , NamedRecord
    , Parser
    , decodeByName
    , (.:)
    )
import qualified Data.Vector as V

import Types.AppError
import Types.Material
import Types.Event


instance FromNamedRecord Material where
    parseNamedRecord r =
        Material
            <$> r .: "material_name"
            <*> r .: "absorption_coefficient"
            <*> parseRemovable r


parseRemovable :: NamedRecord -> Parser Bool
parseRemovable r = do
    value <- r .: "removable" :: Parser String

    case value of
        "true" ->
            pure True

        "false" ->
            pure False

        _ ->
            fail ("Invalid removable value: " ++ value)


instance FromNamedRecord EventRequirement where
    parseNamedRecord r =
        EventRequirement
            <$> (EventType <$> r .: "event_name")
            <*> ((,) <$> r .: "min_reverb" <*> r .: "max_reverb")


loadMaterials :: FilePath -> IO (Either AppError [Material])
loadMaterials filePath = do
    result <- try (BL.readFile filePath)
        :: IO (Either IOException BL.ByteString)

    case result of
        Left err ->
            pure (Left (FileError (show err)))

        Right content ->
            case decodeByName content of
                Left err ->
                    pure (Left (ParseError err))

                Right (_, records) ->
                    pure (Right (V.toList records))


loadEvents :: FilePath -> IO (Either AppError [EventRequirement])
loadEvents filePath = do
    result <- try (BL.readFile filePath)
        :: IO (Either IOException BL.ByteString)

    case result of
        Left err ->
            pure (Left (FileError (show err)))

        Right content ->
            case decodeByName content of
                Left err ->
                    pure (Left (ParseError err))

                Right (_, records) ->
                    pure (Right (V.toList records))


showMaterials :: FilePath -> IO (Either AppError ())
showMaterials filePath = do
    result <- loadMaterials filePath

    case result of
        Left err ->
            pure (Left err)

        Right materials -> do
            putStrLn "Доступные материалы:"

            mapM_ printMaterial (zip [1..] materials)

            pure (Right ())


printMaterial :: (Int, Material) -> IO ()
printMaterial (number, material) =
    putStrLn $
        show number
        ++ ". "
        ++ materialName material
        ++ " (коэффициент поглощения: "
        ++ show (absorptionCoefficient material)
        ++ ")"


getMaterialByNumber
    :: FilePath
    -> Int
    -> IO (Either AppError Material)
getMaterialByNumber filePath number = do
    result <- loadMaterials filePath

    case result of
        Left err ->
            pure (Left err)

        Right materials ->
            case lookup number (zip [1..] materials) of
                Nothing ->
                    pure (Left (NotFound "Материал с таким номером не найден"))

                Just material ->
                    pure (Right material)

getRemovableMaterialByNumber
    :: FilePath
    -> Int
    -> IO (Either AppError Material)
getRemovableMaterialByNumber filePath number = do
    result <- loadMaterials filePath

    case result of
        Left err ->
            pure (Left err)

        Right materials ->
            let removableMaterials =
                    filter removable materials
            in
                case lookup
                    number
                    (zip [1..] removableMaterials) of

                    Nothing ->
                        pure
                            (Left
                                (NotFound
                                    "Регулируемый материал с таким номером не найден"))

                    Just material ->
                        pure (Right material)


showEventRequirements
    :: FilePath
    -> IO (Either AppError ())
showEventRequirements filePath = do
    result <- loadEvents filePath

    case result of
        Left err ->
            pure (Left err)

        Right events -> do
            putStrLn "Доступные типы мероприятий:"

            mapM_ printEvent (zip [1..] events)

            pure (Right ())


printEvent :: (Int, EventRequirement) -> IO ()
printEvent (number, requirement) =
    putStrLn $
        show number
        ++ ". "
        ++ eventName (eventType requirement)
        ++ " (норма RT60: "
        ++ show (fst (recommendedReverb requirement))
        ++ " - "
        ++ show (snd (recommendedReverb requirement))
        ++ " с)"


getEventRequirementByNumber
    :: FilePath
    -> Int
    -> IO (Either AppError EventRequirement)
getEventRequirementByNumber filePath number = do
    result <- loadEvents filePath

    case result of
        Left err ->
            pure (Left err)

        Right events ->
            case lookup number (zip [1..] events) of
                Nothing ->
                    pure (Left (NotFound "Мероприятие с таким номером не найдено"))

                Just event ->
                    pure (Right event)

showRemovableMaterials
    :: FilePath
    -> IO (Either AppError [(Int, Material)])
showRemovableMaterials filePath = do
    result <- loadMaterials filePath

    case result of
        Left err ->
            pure (Left err)

        Right materials -> do
            let removableMaterials =
                    filter removable materials

            putStrLn "Доступные регулируемые материалы:"

            mapM_
                printRemovableMaterial
                (zip [1..] removableMaterials)

            pure
                (Right
                    (zip [1..] removableMaterials))

printRemovableMaterial
    :: (Int, Material)
    -> IO ()
printRemovableMaterial
    (number, material) =
    putStrLn $
        show number
            ++ ". "
            ++ materialName material
            ++ " (коэффициент поглощения: "
            ++ show (absorptionCoefficient material)
            ++ ")"