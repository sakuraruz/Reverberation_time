module Output.Report
    ( generateReport
    , saveReport
    , generateSummaryReport
    , saveSummaryReport
    ) where

import Control.Exception (IOException, try)
import Text.Printf (printf)

import Types.Report
import Types.Room
import Types.Purpose
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation
import Types.Spectrum
import Types.AppError
import Types.Geometry
import Types.Material

generateReport :: Report -> String
generateReport report =
    unlines
        [ header
        , roomSection (reportRoom report)
        , purposeSection (reportPurpose report)
        , "=== РЕЗУЛЬТАТ ДО РЕКОМЕНДАЦИЙ ==="
        , acousticSection (reportAcousticBefore report)
        , suitabilitySection (reportSuitabilityBefore report)
        , recommendationsSection (reportRecommendations report)
        , "=== РЕЗУЛЬТАТ ПОСЛЕ РЕКОМЕНДАЦИЙ ==="
        , acousticSection (reportAcousticAfter report)
        , suitabilitySection (reportSuitabilityAfter report)
        , disclaimer
        ]


header :: String
header =
    unlines
        [ "========================================"
        , "   ОТЧЁТ ПО АКУСТИЧЕСКОЙ ОЦЕНКЕ"
        , "========================================"
        ]

roomSection :: Room -> String
roomSection room =
    unlines
        [ ""
        , "ПОМЕЩЕНИЕ"
        , "---------"
        , "Название: " ++ roomName room
        , "Размеры: "
            ++ printf "%.2f x %.2f x %.2f м"
                (roomLength d) (roomWidth d) (roomHeight d)
        , "Объём: " ++ printf "%.2f м³" (volume d)
        , "Поверхности:"
        ] ++ concatMap formatSurface (roomSurfaces room)
  where
    d = roomDimensions room
    volume dims = roomLength dims * roomWidth dims * roomHeight dims


formatSurface :: Surface -> String
formatSurface surface =
    "  " ++ show (surfaceId surface)
        ++ ": " ++ printf "%.2f м², " (surfaceArea surface)
        ++ surfaceMaterialName (surfaceMaterial surface)
        ++ "\n"

purposeSection :: PurposeReq -> String
purposeSection purpose =
    unlines
        [ ""
        , "НАЗНАЧЕНИЕ ПОМЕЩЕНИЯ"
        , "--------------------"
        , "Тип: " ++ purposeName purpose
        , "Нормативное время реверберации:"
        , formatRangeTable (recommendedReverb purpose)
        ]


formatRangeTable :: ReverbSpectrum -> String
formatRangeTable spectrum =
    "  Частота  | Норма, с\n"
    ++ "  ---------|-----------\n"
    ++ "  125 Гц   | " ++ formatRange (reverbLow  spectrum) ++ "\n"
    ++ "  500 Гц   | " ++ formatRange (reverbMid  spectrum) ++ "\n"
    ++ "  2000 Гц  | " ++ formatRange (reverbHigh spectrum) ++ "\n"


formatRange :: Range -> String
formatRange (Range lo hi) =
    printf "%.2f – %.2f" lo hi

acousticSection :: AcousticResult -> String
acousticSection acoustic =
    unlines
        [ "  Частота  | A, м²·сэбин | T, с"
        , "  ---------|-------------|------"
        , "  125 Гц   | " ++ printf "%.2f" (at125  a)
                     ++ " | " ++ printf "%.2f" (at125  t)
        , "  500 Гц   | " ++ printf "%.2f" (at500  a)
                     ++ " | " ++ printf "%.2f" (at500  t)
        , "  2000 Гц  | " ++ printf "%.2f" (at2000 a)
                     ++ " | " ++ printf "%.2f" (at2000 t)
        ]
  where
    a = totalAbsorption acoustic
    t = reverberationTime acoustic


formatAcousticTable :: AcousticResult -> String
formatAcousticTable acoustic =
    "  Частота  | A, м²·сэбин | T, с\n"
    ++ "  ---------|-------------|------\n"
    ++ formatRow "125 Гц"  (at125  a) (at125  t)
    ++ formatRow "500 Гц"  (at500  a) (at500  t)
    ++ formatRow "2000 Гц" (at2000 a) (at2000 t)
  where
    a = totalAbsorption acoustic
    t = reverberationTime acoustic


formatRow :: String -> Double -> Double -> String
formatRow freq a t =
    printf "  %-8s | %11.2f | %.2f\n" freq a t

suitabilitySection :: SuitabilityResult -> String
suitabilitySection suitability =
    unlines
        [ "  Относительное отклонение: " ++ printf "%.2f" (relativeDev suitability)
        , "  Оценка: " ++ gradeText (grade suitability)
        ]

formatDeviationTable :: SuitabilityResult -> String
formatDeviationTable suitability =
    "  Частота  | Отклонение, с\n"
    ++ "  ---------|---------------\n"
    ++ formatDevRow "125 Гц"  (dev125  d)
    ++ formatDevRow "500 Гц"  (dev500  d)
    ++ formatDevRow "2000 Гц" (dev2000 d)
  where
    d = deviations suitability


formatDevRow :: String -> Double -> String
formatDevRow freq dev =
    printf "  %-8s | %.2f\n" freq dev


gradeText :: Grade -> String
gradeText Suitable              = "ПРИГОДНО"
gradeText ConditionallySuitable = "УСЛОВНО ПРИГОДНО"
gradeText Unsuitable            = "НЕ ПРИГОДНО"

recommendationsSection :: [Recommendation] -> String
recommendationsSection [] =
    "  Рекомендации не требуются.\n"
recommendationsSection recs =
    concatMap formatRec (zip [1..] recs)
  where
    formatRec (i, r) =
        "  " ++ show i ++ ". "
        ++ actionText (action r) ++ " "
        ++ decorName (targetElement r)
        ++ " на " ++ show (targetSurface r)
        ++ " (" ++ printf "%.1f" (recommendedArea r) ++ " м²)\n"


formatRecommendation :: (Int, Recommendation) -> String
formatRecommendation (n, rec) =
    unlines
        [ ""
        , show n ++ ". " ++ actionText (action rec)
            ++ " " ++ decorName (targetElement rec)
        , "   Поверхность: " ++ show (targetSurface rec)
        , "   Площадь: " ++ printf "%.2f м²" (recommendedArea rec)
        , "   T до:    " ++ formatSpectrum (previousReverb rec)
        , "   T после: " ++ formatSpectrum (newReverb rec)
        ]


actionText :: Action -> String
actionText Add    = "Добавить"
actionText Remove = "Убрать"


formatSpectrum :: Spectrum -> String
formatSpectrum s =
    printf "%.2f / %.2f / %.2f с" (at125 s) (at500 s) (at2000 s)

disclaimer :: String
disclaimer =
    unlines
        [ ""
        , "========================================"
        , "ПРИМЕЧАНИЕ"
        , "Расчёт носит оценочный характер."
        , "========================================"
        ]

saveReport :: FilePath -> Report -> IO (Either AppError ())
saveReport filePath report = do
    let content = generateReport report
    result <- try (writeFile filePath content)
        :: IO (Either IOException ())
    case result of
        Left err -> pure (Left (FileError (show err)))
        Right () -> pure (Right ())

generateSummaryReport :: BatchResult -> String
generateSummaryReport result =
    unlines
        [ "========================================"
        , "   СВОДНЫЙ ОТЧЁТ"
        , "========================================"
        , "Всего помещений: " ++ show (brTotal result)
        , "Обработано: " ++ show (brProcessed result)
        , "Пропущено: " ++ show (brSkipped result)
        , ""
        , processedSection (brEntries result)
        , skippedSection (brEntries result)
        ]


processedSection :: [BatchEntry] -> String
processedSection entries =
    let successes = [e | e@BatchSuccess{} <- entries]
    in if null successes
        then "--- Обработанные ---\nНет.\n"
        else "--- Обработанные ---\n"
             ++ concatMap formatEntry (zip [1..] successes)


formatEntry :: (Int, BatchEntry) -> String
formatEntry (n, BatchSuccess name rep) =
    unlines
        [ ""
        , "--- " ++ show n ++ ". " ++ name ++ " ---"
        , ""
        , "Акустика ДО:"
        , acousticSection (reportAcousticBefore rep)
        , suitabilitySection (reportSuitabilityBefore rep)
        , ""
        , "Рекомендации:"
        , recommendationsSection (reportRecommendations rep)
        ]
    ++ afterSection
  where
    afterSection
        | null (reportRecommendations rep) = ""
        | otherwise =
            unlines
                [ ""
                , "Акустика ПОСЛЕ:"
                , acousticSection (reportAcousticAfter rep)
                , suitabilitySection (reportSuitabilityAfter rep)
                ]
formatEntry _ = ""


skippedSection :: [BatchEntry] -> String
skippedSection entries =
    let skipped = [e | e@BatchSkipped{} <- entries]
    in if null skipped
        then "--- Пропущенные ---\nНет.\n"
        else "--- Пропущенные ---\n"
             ++ concatMap formatSkipped (zip [1..] skipped)


formatSkipped :: (Int, BatchEntry) -> String
formatSkipped (n, BatchSkipped name reason) =
    show n ++ ". " ++ name ++ " — " ++ reason ++ "\n"
formatSkipped _ = ""


saveSummaryReport :: FilePath -> BatchResult -> IO (Either AppError ())
saveSummaryReport filePath result = do
    let content = generateSummaryReport result
    r <- try (writeFile filePath content)
        :: IO (Either IOException ())
    case r of
        Left err -> pure (Left (FileError (show err)))
        Right () -> pure (Right ())