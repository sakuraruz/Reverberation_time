module Main where

import Input.CLI
    ( mainMenu
    )


materialsFile :: FilePath
materialsFile = "data/materials.csv"


eventsFile :: FilePath
eventsFile = "data/events.csv"


main :: IO ()
main = do

    result <-
        mainMenu
            materialsFile
            eventsFile

    case result of
        Left err ->
            print err

        Right () ->
            pure ()