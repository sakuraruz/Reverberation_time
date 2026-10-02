{-# LANGUAGE OverloadedStrings #-}

module Types.Geometry
    ( Dimensions(..)
    ) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )


data Dimensions = Dimensions
    { roomLength :: Double
    , roomWidth  :: Double
    , roomHeight :: Double
    } deriving (Show, Eq)


instance ToJSON Dimensions where
    toJSON dimensions =
        object
            [ "roomLength" .= roomLength dimensions
            , "roomWidth"  .= roomWidth dimensions
            , "roomHeight" .= roomHeight dimensions
            ]

instance FromJSON Dimensions where
    parseJSON = withObject "Dimensions" $ \object ->
        Dimensions
            <$> object .: "roomLength"
            <*> object .: "roomWidth"
            <*> object .: "roomHeight"