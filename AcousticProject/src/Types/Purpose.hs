module Types.Purpose
    ( PurposeReq(..)
    ) where

import Types.Spectrum

data PurposeReq = PurposeReq
    { purposeName       :: String
    , recommendedReverb :: ReverbSpectrum
    } deriving (Show, Eq)