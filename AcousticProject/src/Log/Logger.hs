module Log.Logger
    ( LogLevel(..)
    , logMessage
    , logInfo
    , logWarning
    , logError
    ) where

import Control.Exception (IOException, try)

import Types.AppError


data LogLevel
    = Info
    | Warning
    | Error
    deriving (Show, Eq)


logMessage
    :: FilePath
    -> LogLevel
    -> String
    -> IO (Either AppError ())
logMessage filePath level message = do
    result <- try (appendFile filePath logEntry)
        :: IO (Either IOException ())

    case result of
        Left err ->
            pure (Left (FileError (show err)))

        Right () ->
            pure (Right ())
  where
    logEntry =
        "[" ++ show level ++ "] "
            ++ message
            ++ "\n"


logInfo
    :: FilePath
    -> String
    -> IO (Either AppError ())
logInfo filePath message =
    logMessage filePath Info message


logWarning
    :: FilePath
    -> String
    -> IO (Either AppError ())
logWarning filePath message =
    logMessage filePath Warning message


logError
    :: FilePath
    -> String
    -> IO (Either AppError ())
logError filePath message =
    logMessage filePath Error message