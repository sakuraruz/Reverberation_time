module Recommendation.Acoustic
    ( generateRecommendations
    , recommendForReverberation
    ) where

import Data.List (sortOn)
import Data.Ord (Down(..))

import Types.Room
import Types.Event
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation
import qualified Types.Material as Material

import Calculation.Absorption
import Calculation.Reverberation
import Evaluation.Suitability


generateRecommendations :: Room -> EventRequirement -> AcousticResult -> SuitabilityResult -> [Recommendation]
generateRecommendations room event acoustic suitability
    | suitable suitability = []
    | otherwise =
        recommendForReverberation
            room
            event
            acoustic


recommendForReverberation :: Room -> EventRequirement -> AcousticResult -> [Recommendation]
recommendForReverberation room event acoustic
    | currentReverb > maximumReverb =
        recommendAdd
            event
            acoustic
            sortedItems

    | currentReverb < minimumReverb =
        recommendRemove
            event
            acoustic
            sortedItems

    | otherwise = []
  where
    currentReverb =
        reverberationTime acoustic

    (minimumReverb, maximumReverb) =
        recommendedReverb event

    sortedItems =
        sortOn
            (Down . calculateItemAbsorption)
            (adjustableItems room)


recommendAdd :: EventRequirement -> AcousticResult -> [AdjustableItem] -> [Recommendation]
recommendAdd event acoustic items
    | isWithinRange
        (reverberationTime acoustic)
        (recommendedReverb event) =
        []

    | otherwise =
        case items of
            [] ->
                []

            item : remainingItems ->
                let recommendation =
                        makeIncreaseRecommendation
                            event
                            acoustic
                            item

                    newAcoustic =
                        AcousticResult
                            { roomVolume =
                                roomVolume acoustic
                            , absorptionArea =
                                newAbsorption
                            , reverberationTime =
                                newReverb
                            }

                    newAbsorption =
                        absorptionArea acoustic
                            + calculateItemAbsorption item

                    newReverb =
                        calculateReverberationTime
                            (roomVolume acoustic)
                            newAbsorption

                in recommendation
                    : recommendAdd
                        event
                        newAcoustic
                        remainingItems


recommendRemove :: EventRequirement -> AcousticResult -> [AdjustableItem] -> [Recommendation]
recommendRemove event acoustic items
    | isWithinRange
        (reverberationTime acoustic)
        (recommendedReverb event) =
        []

    | otherwise =
        case items of
            [] ->
                []

            item : remainingItems ->
                let recommendation =
                        makeDecreaseRecommendation
                            event
                            acoustic
                            item

                    newAcoustic =
                        AcousticResult
                            { roomVolume =
                                roomVolume acoustic
                            , absorptionArea =
                                newAbsorption
                            , reverberationTime =
                                newReverb
                            }

                    newAbsorption =
                        max
                            0
                            (absorptionArea acoustic
                                - calculateItemAbsorption item)

                    newReverb =
                        calculateReverberationTime
                            (roomVolume acoustic)
                            newAbsorption

                in recommendation
                    : recommendRemove
                        event
                        newAcoustic
                        remainingItems


makeIncreaseRecommendation :: EventRequirement -> AcousticResult -> AdjustableItem -> Recommendation
makeIncreaseRecommendation event acoustic item =
    Recommendation
        { action =
            "Добавить " ++ itemName item

        , materialName =
            Material.materialName
                (itemMaterial item)

        , previousReverb =
            oldReverb

        , newReverb =
            newReverb

        , result =
            newSuitability
        }
  where
    oldReverb =
        reverberationTime acoustic

    newAbsorption =
        absorptionArea acoustic
            + calculateItemAbsorption item

    newReverb =
        calculateReverberationTime
            (roomVolume acoustic)
            newAbsorption

    newAcoustic =
        AcousticResult
            { roomVolume =
                roomVolume acoustic
            , absorptionArea =
                newAbsorption
            , reverberationTime =
                newReverb
            }

    newSuitability =
        evaluateSuitability
            newAcoustic
            event


makeDecreaseRecommendation :: EventRequirement -> AcousticResult -> AdjustableItem -> Recommendation
makeDecreaseRecommendation event acoustic item =
    Recommendation
        { action =
            "Убрать " ++ itemName item

        , materialName =
            Material.materialName
                (itemMaterial item)

        , previousReverb =
            oldReverb

        , newReverb =
            newReverb

        , result =
            newSuitability
        }
  where
    oldReverb =
        reverberationTime acoustic

    newAbsorption =
        max
            0
            (absorptionArea acoustic
                - calculateItemAbsorption item)

    newReverb =
        calculateReverberationTime
            (roomVolume acoustic)
            newAbsorption

    newAcoustic =
        AcousticResult
            { roomVolume =
                roomVolume acoustic
            , absorptionArea =
                newAbsorption
            , reverberationTime =
                newReverb
            }

    newSuitability =
        evaluateSuitability
            newAcoustic
            event