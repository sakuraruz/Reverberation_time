{-# LANGUAGE OverloadedStrings #-}

module Types.Room
    (SurfaceId(..)
    , Door(..)
    , Window(..)
    , DecorItem(..)
    , SeatItem(..)
    , SurfaceElem(..)
    , Surface(..)
    , Room(..)
    ) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )

import Types.Material
import Types.Geometry

data SurfaceId
    = Wall Int
    | Floor
    | Ceiling
    deriving (Show, Eq, Ord)


instance ToJSON SurfaceId where
    toJSON (Wall n) = object ["tag" .= ("Wall" :: String), "contents" .= n]
    toJSON Floor    = object ["tag" .= ("Floor" :: String)]
    toJSON Ceiling  = object ["tag" .= ("Ceiling" :: String)]

instance FromJSON SurfaceId where
    parseJSON = withObject "SurfaceId" $ \o -> do
        tag <- o .: "tag"
        case tag :: String of
            "Wall"    -> Wall    <$> o .: "contents"
            "Floor"   -> pure Floor
            "Ceiling" -> pure Ceiling
            _         -> fail ("Unknown SurfaceId: " ++ tag)

data Door = Door
    { doorId       :: String
    , doorMaterial :: DoorMaterial
    , doorArea     :: Double
    } deriving (Show, Eq)


instance ToJSON Door where
    toJSON d = object
        [ "doorId"       .= doorId d
        , "doorMaterial" .= doorMaterial d
        , "doorArea"     .= doorArea d
        ]

instance FromJSON Door where
    parseJSON = withObject "Door" $ \o ->
        Door
            <$> o .: "doorId"
            <*> o .: "doorMaterial"
            <*> o .: "doorArea"

data Window = Window
    { windowId       :: String
    , windowMaterial :: WindowMaterial
    , windowArea     :: Double
    } deriving (Show, Eq)


instance ToJSON Window where
    toJSON w = object
        [ "windowId"       .= windowId w
        , "windowMaterial" .= windowMaterial w
        , "windowArea"     .= windowArea w
        ]

instance FromJSON Window where
    parseJSON = withObject "Window" $ \o ->
        Window
            <$> o .: "windowId"
            <*> o .: "windowMaterial"
            <*> o .: "windowArea"

data DecorItem = DecorItem
    { decorMaterial :: DecorMaterial
    , decorArea     :: Double
    } deriving (Show, Eq)


instance ToJSON DecorItem where
    toJSON d = object
        [ "decorMaterial" .= decorMaterial d
        , "decorArea"     .= decorArea d
        ]

instance FromJSON DecorItem where
    parseJSON = withObject "DecorItem" $ \o ->
        DecorItem
            <$> o .: "decorMaterial"
            <*> o .: "decorArea"

data SeatItem = SeatItem
    { seatCount    :: Int
    , seatMaterial :: SeatMaterial
    } deriving (Show, Eq)


instance ToJSON SeatItem where
    toJSON s = object
        [ "seatCount"    .= seatCount s
        , "seatMaterial" .= seatMaterial s
        ]

instance FromJSON SeatItem where
    parseJSON = withObject "SeatItem" $ \o ->
        SeatItem
            <$> o .: "seatCount"
            <*> o .: "seatMaterial"

data SurfaceElem
    = ElemDoor   Door
    | ElemWindow Window
    | ElemDecor  DecorItem
    deriving (Show, Eq)


instance ToJSON SurfaceElem where
    toJSON (ElemDoor   d) = object ["type" .= ("door"   :: String), "value" .= d]
    toJSON (ElemWindow w) = object ["type" .= ("window" :: String), "value" .= w]
    toJSON (ElemDecor  d) = object ["type" .= ("decor"  :: String), "value" .= d]

instance FromJSON SurfaceElem where
    parseJSON = withObject "SurfaceElem" $ \o -> do
        t <- o .: "type"
        case t :: String of
            "door"   -> ElemDoor   <$> o .: "value"
            "window" -> ElemWindow <$> o .: "value"
            "decor"  -> ElemDecor  <$> o .: "value"
            _        -> fail ("Unknown SurfaceElem: " ++ t)

data Surface = Surface
    { surfaceId       :: SurfaceId
    , surfaceMaterial :: SurfaceMaterial
    , surfaceArea     :: Double
    , surfaceElems    :: [SurfaceElem]
    } deriving (Show, Eq)


instance ToJSON Surface where
    toJSON s = object
        [ "surfaceId"       .= surfaceId s
        , "surfaceMaterial" .= surfaceMaterial s
        , "surfaceArea"     .= surfaceArea s
        , "surfaceElems"    .= surfaceElems s
        ]

instance FromJSON Surface where
    parseJSON = withObject "Surface" $ \o ->
        Surface
            <$> o .: "surfaceId"
            <*> o .: "surfaceMaterial"
            <*> o .: "surfaceArea"
            <*> o .: "surfaceElems"

data Room = Room
    { roomName       :: String
    , roomDimensions :: Dimensions
    , roomSurfaces   :: [Surface]
    , roomSeats      :: [SeatItem]
    } deriving (Show, Eq)


instance ToJSON Room where
    toJSON r = object
        [ "roomName"       .= roomName r
        , "roomDimensions" .= roomDimensions r
        , "roomSurfaces"   .= roomSurfaces r
        , "roomSeats"      .= roomSeats r
        ]

instance FromJSON Room where
    parseJSON = withObject "Room" $ \o ->
        Room
            <$> o .: "roomName"
            <*> o .: "roomDimensions"
            <*> o .: "roomSurfaces"
            <*> o .: "roomSeats"