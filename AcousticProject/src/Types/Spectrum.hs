{-# LANGUAGE OverloadedStrings #-}

module Types.Spectrum where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )


data Spectrum = Spectrum
    { at125  :: Double
    , at500  :: Double
    , at2000 :: Double
    } deriving (Show, Eq)


instance ToJSON Spectrum where
    toJSON s = object
        [ "at125"  .= at125 s
        , "at500"  .= at500 s
        , "at2000" .= at2000 s
        ]


instance FromJSON Spectrum where
    parseJSON = withObject "Spectrum" $ \o ->
        Spectrum
            <$> o .: "at125"
            <*> o .: "at500"
            <*> o .: "at2000"

data Range = Range
    { rangeMin :: Double
    , rangeMax :: Double
    } deriving (Show, Eq)

data ReverbSpectrum = ReverbSpectrum
    { reverbLow  :: Range
    , reverbMid  :: Range
    , reverbHigh :: Range
    } deriving (Show, Eq)


instance ToJSON Range where
    toJSON r = object
        [ "rangeMin" .= rangeMin r
        , "rangeMax" .= rangeMax r
        ]


instance FromJSON Range where
    parseJSON = withObject "Range" $ \o ->
        Range
            <$> o .: "rangeMin"
            <*> o .: "rangeMax"

instance ToJSON ReverbSpectrum where
    toJSON r = object
        [ "reverbLow"  .= reverbLow r
        , "reverbMid"  .= reverbMid r
        , "reverbHigh" .= reverbHigh r
        ]


instance FromJSON ReverbSpectrum where
    parseJSON = withObject "ReverbSpectrum" $ \o ->
        ReverbSpectrum
            <$> o .: "reverbLow"
            <*> o .: "reverbMid"
            <*> o .: "reverbHigh"

addSpectrums :: Spectrum -> Spectrum -> Spectrum
addSpectrums a b = Spectrum
    { at125  = at125 a + at125 b
    , at500  = at500 a + at500 b
    , at2000 = at2000 a + at2000 b
    }

subtractSpectrums :: Spectrum -> Spectrum -> Spectrum
subtractSpectrums a b = Spectrum
    { at125  = at125 a - at125 b
    , at500  = at500 a - at500 b
    , at2000 = at2000 a - at2000 b
    }

multiplySpectrum :: Spectrum -> Double -> Spectrum
multiplySpectrum s k = Spectrum
    { at125  = at125 s * k
    , at500  = at500 s * k
    , at2000 = at2000 s * k
    }

divideSpectrum :: Spectrum -> Double -> Spectrum
divideSpectrum s k = Spectrum
    { at125  = at125 s / k
    , at500  = at500 s / k
    , at2000 = at2000 s / k
    }

sumSpectrums :: [Spectrum] -> Spectrum
sumSpectrums = foldr addSpectrums (Spectrum 0 0 0)

mapSpectrum :: (Double -> Double) -> Spectrum -> Spectrum
mapSpectrum f s = Spectrum
    { at125  = f (at125 s)
    , at500  = f (at500 s)
    , at2000 = f (at2000 s)
    }

negateSpectrum :: Spectrum -> Spectrum
negateSpectrum = mapSpectrum negate