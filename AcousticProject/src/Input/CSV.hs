{-# LANGUAGE OverloadedStrings #-}

module Input.CSV
    ( loadSurfaceMaterials
    , loadDoorMaterials
    , loadWindowMaterials
    , loadWallDecor
    , loadFloorDecor
    , loadSeatMaterials
    , loadPurpose
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
    ( AppError(..)
    )
import Types.Spectrum
import Types.Material
import Types.Purpose

instance FromNamedRecord Spectrum where
    parseNamedRecord r =
        Spectrum
            <$> r .: "alpha_125"
            <*> r .: "alpha_500"
            <*> r .: "alpha_2000"

instance FromNamedRecord SurfaceMaterial where
    parseNamedRecord r =
        SurfaceMaterial
            <$> r .: "material_name"
            <*> parseNamedRecord r


loadSurfaceMaterials :: FilePath -> IO (Either AppError [SurfaceMaterial])
loadSurfaceMaterials = loadCsv

instance FromNamedRecord DoorMaterial where
    parseNamedRecord r =
        DoorMaterial
            <$> r .: "door_name"
            <*> parseNamedRecord r


loadDoorMaterials :: FilePath -> IO (Either AppError [DoorMaterial])
loadDoorMaterials = loadCsv

instance FromNamedRecord WindowMaterial where
    parseNamedRecord r =
        WindowMaterial
            <$> r .: "window_name"
            <*> parseNamedRecord r


loadWindowMaterials :: FilePath -> IO (Either AppError [WindowMaterial])
loadWindowMaterials = loadCsv

instance FromNamedRecord DecorMaterial where
    parseNamedRecord r =
        DecorMaterial
            <$> r .: "decor_name"
            <*> parseNamedRecord r


loadWallDecor :: FilePath -> IO (Either AppError [DecorMaterial])
loadWallDecor = loadCsv


loadFloorDecor :: FilePath -> IO (Either AppError [DecorMaterial])
loadFloorDecor = loadCsv

instance FromNamedRecord SeatMaterial where
    parseNamedRecord r =
        SeatMaterial
            <$> r .: "seat_name"
            <*> parseEquivalentArea r
            <*> r .: "floor_area"


parseEquivalentArea :: NamedRecord -> Parser Spectrum
parseEquivalentArea r =
    Spectrum
        <$> r .: "area_125"
        <*> r .: "area_500"
        <*> r .: "area_2000"


loadSeatMaterials :: FilePath -> IO (Either AppError [SeatMaterial])
loadSeatMaterials = loadCsv

instance FromNamedRecord PurposeReq where
    parseNamedRecord r =
        PurposeReq
            <$> r .: "purpose_name"
            <*> parseReverbSpectrum r


parseReverbSpectrum :: NamedRecord -> Parser ReverbSpectrum
parseReverbSpectrum r =
    ReverbSpectrum
        <$> (Range <$> r .: "min_125"  <*> r .: "max_125")
        <*> (Range <$> r .: "min_500"  <*> r .: "max_500")
        <*> (Range <$> r .: "min_2000" <*> r .: "max_2000")


loadPurpose :: FilePath -> IO (Either AppError [PurposeReq])
loadPurpose = loadCsv

loadCsv :: FromNamedRecord a => FilePath -> IO (Either AppError [a])
loadCsv filePath = do
    readResult <- try (BL.readFile filePath)
        :: IO (Either IOException BL.ByteString)
    case readResult of
        Left err -> pure (Left (FileError (show err)))
        Right content ->
            case decodeByName content of
                Left err -> pure (Left (ParseError err))
                Right (_, records) -> pure (Right (V.toList records))