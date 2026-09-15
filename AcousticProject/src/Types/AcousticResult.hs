module Types.AcousticResult where

data AcousticResult = AcousticResult
    { roomVolume        :: Double
    , absorptionArea    :: Double
    , reverberationTime :: Double
    } deriving (Show, Eq)