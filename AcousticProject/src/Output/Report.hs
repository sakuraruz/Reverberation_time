module Output.Report
    ( generateReport
    , saveReport
    ) where

import Control.Exception (IOException, try)

import Types.Report
import Types.Recommendation
import Types.Suitability
import Types.Room
import Types.AcousticResult
import Types.Event
import Types.AppError


generateReport :: Report -> String
generateReport report =
    unlines
        [ "ОТЧЁТ ПО АКУСТИЧЕСКОЙ ОЦЕНКЕ"
        , ""
        , "Помещение: " ++ roomName (reportRoom report)
        , "Мероприятие: "
            ++ eventName (eventType (reportEvent report))
        , ""
        , "Исходные акустические параметры:"
        , "Объём помещения: "
            ++ show (roomVolume (reportAcoustic report))
            ++ " м³"
        , "Эквивалентная площадь звукопоглощения: "
            ++ show (absorptionArea (reportAcoustic report))
            ++ " м²"
        , "Время реверберации: "
            ++ show (reverberationTime (reportAcoustic report))
            ++ " с"
        , "Нормативное время реверберации для мероприятия: "
            ++ show (fst (recommendedReverb (reportEvent report)))
            ++ "–"
            ++ show (snd (recommendedReverb (reportEvent report)))
            ++ " с"
        , ""
        , "Исходная оценка:"
        , suitabilityText (reportSuitability report)
        , ""
        , "РЕКОМЕНДАЦИИ:"
        , recommendationsText
        , ""
        , finalResult report
        ]
  where
    recommendationsText =
        if null (reportRecommendations report)
            then "Рекомендации не требуются."
            else
                unlines
                    (map formatRecommendation
                        (zip [1..] (reportRecommendations report)))


formatRecommendation :: (Int, Recommendation) -> String
formatRecommendation (number, recommendation) =
    unlines
        [ show number
            ++ ". "
            ++ action recommendation
        , "   Материал: "
            ++ materialName recommendation
        , "   Время реверберации: "
            ++ show (previousReverb recommendation)
            ++ " → "
            ++ show (newReverb recommendation)
            ++ " с"
        , "   Результат: "
            ++ suitabilityText (result recommendation)
        ]


suitabilityText :: SuitabilityResult -> String
suitabilityText suitability
    | suitable suitability =
        "Соответствует нормативным требованиям."
    | otherwise =
        "Не соответствует нормативным требованиям. "
            ++ "Отклонение: "
            ++ show (deviation suitability)
            ++ " с."


finalResult :: Report -> String
finalResult report =
    case reportRecommendations report of
        [] ->
            if suitable (reportSuitability report)
                then
                    "ИТОГ:\n"
                    ++ "Помещение соответствует требованиям."
                else
                    "ИТОГ:\n"
                    ++ "Для помещения не найдено подходящих "
                    ++ "рекомендаций."

        recommendations ->
            if suitable
                (result (last recommendations))
            then
                "ИТОГ:\n"
                ++ "После применения рекомендаций "
                ++ "помещение соответствует требованиям."
            else
                "ИТОГ:\n"
                ++ "Помещение не соответствует требованиям.\n"
                ++ "Даже после применения всех доступных "
                ++ "рекомендаций нормативное время "
                ++ "реверберации не достигнуто."


saveReport :: FilePath -> Report -> IO (Either AppError ())
saveReport filePath report = do
    let content =
            generateReport report

    result <-
        try (writeFile filePath content)
            :: IO (Either IOException ())

    case result of
        Left err ->
            pure (Left (FileError (show err)))

        Right () ->
            pure (Right ())