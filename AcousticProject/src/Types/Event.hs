module Types.Event where

data EventType = EventType
    { eventName :: String
    } deriving (Show, Eq)

data EventRequirement = EventRequirement
    { eventType         :: EventType
    , recommendedReverb :: (Double, Double)
    } deriving (Show, Eq)