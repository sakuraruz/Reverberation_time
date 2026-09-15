module Types.Report where

import Types.Room
import Types.Event
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation

data Report = Report
    { reportRoom            :: Room
    , reportEvent           :: EventRequirement
    , reportAcoustic        :: AcousticResult
    , reportSuitability     :: SuitabilityResult
    , reportRecommendations :: [Recommendation]
    } deriving (Show, Eq)