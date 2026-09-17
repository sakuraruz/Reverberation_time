module Recommendation.AcousticSpec where

import Test.QuickCheck

import qualified Types.Material as M
import Types.Room
import Types.Geometry
import Types.Event
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation

import Recommendation.Acoustic


carpet :: M.Material
carpet =
    M.Material
        { M.materialName = "Ковролин"
        , M.absorptionCoefficient = 0.35
        , M.removable = True
        }


curtains :: M.Material
curtains =
    M.Material
        { M.materialName = "Шторы"
        , M.absorptionCoefficient = 0.50
        , M.removable = True
        }


roomWithItems :: Room
roomWithItems =
    Room
        { roomName = "Тестовая комната"
        , dimensions = Dimensions 5 4 3
        , surfaces = []
        , adjustableItems =
            [ AdjustableItem
                { itemName = "Ковролин"
                , itemMaterial = carpet
                , itemArea = 12
                }

            , AdjustableItem
                { itemName = "Шторы"
                , itemMaterial = curtains
                , itemArea = 8
                }
            ]
        }


lecture :: EventRequirement
lecture =
    EventRequirement
        { eventType = EventType "Лекция"
        , recommendedReverb = (0.8, 1.2)
        }


prop_suitableRoomHasNoRecommendations
    :: Positive Double
    -> Bool
prop_suitableRoomHasNoRecommendations
    (Positive absorption) =

        let acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = absorption
                    , reverberationTime = 1.0
                    }

            suitability =
                SuitabilityResult
                    { suitable = True
                    , deviation = 0
                    }

        in generateRecommendations
            roomWithItems
            lecture
            acoustic
            suitability
            == []


prop_noItemsMeansNoRecommendations
    :: Positive Double
    -> Bool
prop_noItemsMeansNoRecommendations
    (Positive absorption) =

        let room =
                roomWithItems
                    { adjustableItems = []
                    }

            acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = absorption
                    , reverberationTime = 2.0
                    }

            suitability =
                SuitabilityResult
                    { suitable = False
                    , deviation = 0.8
                    }

        in generateRecommendations
            room
            lecture
            acoustic
            suitability
            == []


prop_highReverberationProducesRecommendations
    :: Positive Double
    -> Bool
prop_highReverberationProducesRecommendations
    (Positive x) =

        let deviation =
                0.1 * x / (1 + x)

            acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 4
                    , reverberationTime = 1.2 + 0.1 * x / (1 + x)
                    }

            recommendations =
                recommendForReverberation
                    roomWithItems
                    lecture
                    acoustic

        in not (null recommendations)


prop_lowReverberationProducesRecommendations
    :: Positive Double
    -> Bool
prop_lowReverberationProducesRecommendations
    (Positive x) =

        let deviation =
                0.1 * x / (1 + x)

            acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 20
                    , reverberationTime = 0.8 - 0.1 * x / (1 + x)
                    }

            recommendations =
                recommendForReverberation
                    roomWithItems
                    lecture
                    acoustic

        in not (null recommendations)


prop_normalReverberationProducesNoRecommendations
    :: Positive Double
    -> Bool
prop_normalReverberationProducesNoRecommendations
    (Positive x) =

        let reverb =
                0.8
                    + (x / (1 + x))
                    * 0.4

            acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 10
                    , reverberationTime = reverb
                    }

        in recommendForReverberation
            roomWithItems
            lecture
            acoustic
            == []


prop_highReverberationRecommendsAdding
    :: Positive Double
    -> Bool
prop_highReverberationRecommendsAdding
    (Positive x) =

        let acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 4
                    , reverberationTime = 1.2 + x
                    }

            recommendations =
                recommendForReverberation
                    roomWithItems
                    lecture
                    acoustic

        in case recommendations of
            [] ->
                False

            firstRecommendation : _ ->
                take 8 (action firstRecommendation)
                    == "Добавить"


prop_lowReverberationRecommendsRemoving
    :: Positive Double
    -> Bool
prop_lowReverberationRecommendsRemoving
    (Positive x) =

        let acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 20
                    , reverberationTime = 0.8 - x
                    }

            recommendations =
                recommendForReverberation
                    roomWithItems
                    lecture
                    acoustic

        in case recommendations of
            [] ->
                False

            firstRecommendation : _ ->
                take 6 (action firstRecommendation)
                    == "Убрать"


prop_recommendationChangesReverberation
    :: Positive Double
    -> Bool
prop_recommendationChangesReverberation
    (Positive x) =

        let acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 4
                    , reverberationTime = 1.2 + 0.1 * x / (1 + x)
                    }

            recommendations =
                recommendForReverberation
                    roomWithItems
                    lecture
                    acoustic

        in case recommendations of
            [] ->
                False

            firstRecommendation : _ ->
                newReverb firstRecommendation
                    /= previousReverb firstRecommendation


prop_recommendationContainsMaterialName
    :: Positive Double
    -> Bool
prop_recommendationContainsMaterialName
    (Positive x) =

        let acoustic =
                AcousticResult
                    { roomVolume = 60
                    , absorptionArea = 4
                    , reverberationTime = 1.2 + 0.1 * x / (1 + x)
                    }

            recommendations =
                recommendForReverberation
                    roomWithItems
                    lecture
                    acoustic

        in case recommendations of
            [] ->
                False

            firstRecommendation : _ ->
                materialName firstRecommendation
                    `elem`
                        [ "Ковролин"
                        , "Шторы"
                        ]