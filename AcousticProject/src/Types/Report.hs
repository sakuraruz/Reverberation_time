module Types.Report
    (Report(..)
    , BatchResult(..)
    , BatchEntry(..)
    ) where

import Types.Room
import Types.Purpose
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation

data Report = Report
    { reportRoom              :: Room
    , reportPurpose           :: PurposeReq
    , reportAcousticBefore    :: AcousticResult
    , reportSuitabilityBefore :: SuitabilityResult
    , reportAcousticAfter     :: AcousticResult
    , reportSuitabilityAfter  :: SuitabilityResult
    , reportRecommendations   :: [Recommendation]
    } deriving (Show, Eq)

data BatchEntry
    = BatchSuccess
        { beRoomName        :: String
        , beReport          :: Report
        }
    | BatchSkipped
        { beRoomName :: String
        , beReason   :: String
        }
    deriving (Show, Eq)


data BatchResult = BatchResult
    { brTotal     :: Int
    , brProcessed :: Int
    , brSkipped   :: Int
    , brEntries   :: [BatchEntry]
    } deriving (Show, Eq)