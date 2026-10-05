module Log.Logger
    ( LogLevel(..)
    , logMessage
    , logInfo
    , logWarning
    , logError
    , initLog
    ) where

import Control.Exception (IOException, try)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (doesFileExist)

import Types.AppError

data LogLevel
    = Info
    | Warning
    | Error
    deriving (Show, Eq, Ord)

levelText :: LogLevel -> String
levelText Info    = "INFO"
levelText Warning = "WARN"
levelText Error   = "ERROR"

initLog :: FilePath -> IO (Either AppError ())
initLog filePath = do
    exists <- doesFileExist filePath
    if exists
    then pure (Right ())
    else do
        result <- try (writeFile filePath header)
            :: IO (Either IOException ())
        case result of
            Left err -> pure (Left (FileError (show err)))
            Right () -> pure (Right ())
  where
    header =
        "========================================\n"
        ++ "  ЖУРНАЛ РАБОТЫ ПРОГРАММЫ\n"
        ++ "========================================\n"

logMessage
    :: FilePath
    -> LogLevel
    -> String
    -> IO (Either AppError ())
logMessage filePath level message = do
    timestamp <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
        entry   = "[" ++ timeStr ++ "] ["
                    ++ levelText level ++ "] "
                    ++ message ++ "\n"

    result <- try (appendFile filePath entry)
        :: IO (Either IOException ())

    case result of
        Left err -> pure (Left (FileError (show err)))
        Right () -> pure (Right ())

logInfo :: FilePath -> String -> IO (Either AppError ())
logInfo filePath = logMessage filePath Info


logWarning :: FilePath -> String -> IO (Either AppError ())
logWarning filePath = logMessage filePath Warning


logError :: FilePath -> String -> IO (Either AppError ())
logError filePath = logMessage filePath Error