module Types.AppError where

data AppError
    = FileError    String
    | ParseError   String
    | InvalidInput String
    | NotFound     String
    deriving (Show, Eq)