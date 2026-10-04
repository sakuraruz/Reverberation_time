module Recommendation.Acoustic
    ( generateRecommendations
    , isAchievable
    , applyRecommendations
    , recalculateAcoustic
    ) where

import Data.List (maximumBy)
import Data.Ord (comparing)

import Types.Spectrum
import Types.Material
import Types.Room
import Types.Purpose
import Types.AcousticResult
import Types.Suitability
import Types.Recommendation

import Calculation.Geometry
import Calculation.Absorption
import Calculation.Reverberation
import Evaluation.Suitability

data Frequency = F125 | F500 | F2000
    deriving (Show, Eq)

absorptionAt :: AcousticResult -> Frequency -> Double
absorptionAt a F125  = at125  (totalAbsorption a)
absorptionAt a F500  = at500  (totalAbsorption a)
absorptionAt a F2000 = at2000 (totalAbsorption a)


reverberationAt :: AcousticResult -> Frequency -> Double
reverberationAt a F125  = at125  (reverberationTime a)
reverberationAt a F500  = at500  (reverberationTime a)
reverberationAt a F2000 = at2000 (reverberationTime a)


maxReverbAt :: PurposeReq -> Frequency -> Double
maxReverbAt p F125  = rangeMax (reverbLow  (recommendedReverb p))
maxReverbAt p F500  = rangeMax (reverbMid  (recommendedReverb p))
maxReverbAt p F2000 = rangeMax (reverbHigh (recommendedReverb p))


minReverbAt :: PurposeReq -> Frequency -> Double
minReverbAt p F125  = rangeMin (reverbLow  (recommendedReverb p))
minReverbAt p F500  = rangeMin (reverbMid  (recommendedReverb p))
minReverbAt p F2000 = rangeMin (reverbHigh (recommendedReverb p))

absorptionTarget :: AcousticResult -> Double -> Double
absorptionTarget acoustic t =
    let v = roomVolume acoustic
        s = commonArea acoustic
        x = 0.163 * v / t
    in if s <= 0 || t <= 0
       then 0
       else if x / s <= 0.2
            then x
            else s * (1 - exp (negate (x / s)))

absorptionBounds :: AcousticResult -> PurposeReq -> Frequency -> (Double, Double)
absorptionBounds acoustic purpose freq =
    let a_min = absorptionTarget acoustic (maxReverbAt purpose freq)
        a_max = absorptionTarget acoustic (minReverbAt purpose freq)
    in (a_min, a_max)

deltaRequired :: AcousticResult -> PurposeReq -> Frequency -> Double
deltaRequired acoustic purpose freq =
    let a_f    = absorptionAt acoustic freq
        (a_min, a_max) = absorptionBounds acoustic purpose freq
        t_f    = reverberationAt acoustic freq
        t_min  = minReverbAt purpose freq
        t_max  = maxReverbAt purpose freq
    in if t_f > t_max
       then a_min - a_f
       else if t_f < t_min
            then a_max - a_f
            else 0

allowedInterval :: AcousticResult -> PurposeReq -> Frequency -> (Double, Double)
allowedInterval acoustic purpose freq =
    let a_f = absorptionAt acoustic freq
        (a_min, a_max) = absorptionBounds acoustic purpose freq
        l_f = min a_f a_min
        u_f = max a_f a_max
    in (l_f, u_f)

areaLimitAt :: AcousticResult -> PurposeReq -> DecorMaterial -> Surface -> Frequency -> Double
areaLimitAt acoustic purpose decor surface freq =
    let a_f = absorptionAt acoustic freq
        d_f = deltaOnFrequency decor surface freq
        (l_f, u_f) = allowedInterval acoustic purpose freq
    in if d_f > 0
       then (u_f - a_f) / d_f
       else if d_f < 0
            then (a_f - l_f) / (-d_f)
            else 1 / 0


limitArea :: AcousticResult -> PurposeReq -> DecorMaterial -> Surface -> Double
limitArea acoustic purpose decor surface =
    minimum
        [ areaLimitAt acoustic purpose decor surface F125
        , areaLimitAt acoustic purpose decor surface F500
        , areaLimitAt acoustic purpose decor surface F2000
        ]

requiredArea :: AcousticResult -> PurposeReq -> Frequency -> DecorMaterial -> Surface -> Double
requiredArea acoustic purpose freq decor surface =
    let deltaA = deltaRequired acoustic purpose freq
        d_f    = deltaOnFrequency decor surface freq
    in if d_f == 0
       then 0
       else abs deltaA / abs d_f

deltaOnFrequency :: DecorMaterial -> Surface -> Frequency -> Double
deltaOnFrequency decor surface F125 =
    at125 (decorAbsorption decor)
    - at125 (materialAbsorption (surfaceMaterial surface))
deltaOnFrequency decor surface F500 =
    at500 (decorAbsorption decor)
    - at500 (materialAbsorption (surfaceMaterial surface))
deltaOnFrequency decor surface F2000 =
    at2000 (decorAbsorption decor)
    - at2000 (materialAbsorption (surfaceMaterial surface))

findWorstFrequency :: AcousticResult -> PurposeReq -> Frequency
findWorstFrequency acoustic purpose =
    let r125  = relativeAt (reverberationAt acoustic F125)  (minReverbAt purpose F125,  maxReverbAt purpose F125)
        r500  = relativeAt (reverberationAt acoustic F500)  (minReverbAt purpose F500,  maxReverbAt purpose F500)
        r2000 = relativeAt (reverberationAt acoustic F2000) (minReverbAt purpose F2000, maxReverbAt purpose F2000)
        pairs = [(F125, r125), (F500, r500), (F2000, r2000)]
    in fst (maximumBy (comparing snd) pairs)
  where
    relativeAt value (lo, hi)
        | value < lo = (lo - value) / lo
        | value > hi = (value - hi) / hi
        | otherwise  = 0

generateRecommendations
    :: Room
    -> PurposeReq
    -> AcousticResult
    -> SuitabilityResult
    -> [DecorMaterial]
    -> [DecorMaterial]
    -> [Recommendation]
generateRecommendations room purpose acoustic suitability wallCat floorCat
    | suitable suitability = []
    | not (isAchievable room purpose acoustic (wallCat ++ floorCat)) = []
    | otherwise = recommendLoop room purpose acoustic suitability
                                  wallCat floorCat []

recommendLoop :: Room -> PurposeReq -> AcousticResult -> SuitabilityResult -> [DecorMaterial] -> [DecorMaterial] -> [Recommendation] -> [Recommendation]
recommendLoop room purpose acoustic suitability wallCat floorCat acc
    | suitable suitability = reverse acc
    | otherwise =
        case chooseBestPair room purpose acoustic wallCat floorCat of
            Nothing -> reverse acc
            Just (decor, sid, area) ->
                let change         = AddDecor sid decor area
                    newRoom        = applyChange change room
                    newAcoustic    = recalculateAcoustic newRoom
                    newSuitability = evaluateSuitability newAcoustic purpose
                    recommendation = buildRecommendation decor sid area
                                        acoustic newAcoustic newSuitability
                    newWallCat     = filter (\x -> decorName x /= decorName decor) wallCat
                    newFloorCat    = filter (\x -> decorName x /= decorName decor) floorCat
                in recommendLoop newRoom purpose newAcoustic newSuitability
                                newWallCat newFloorCat
                                (recommendation : acc)

isAchievable :: Room -> PurposeReq -> AcousticResult -> [DecorMaterial] -> Bool
isAchievable _ _ _ [] = False
isAchievable _ _ _ _  = True

chooseBestPair :: Room -> PurposeReq -> AcousticResult -> [DecorMaterial] -> [DecorMaterial] -> Maybe (DecorMaterial, SurfaceId, Double)
chooseBestPair room purpose acoustic wallCat floorCat =
    let worst = findWorstFrequency acoustic purpose

        wallCandidates =
            concatMap (candidatesFor worst [Wall 1, Wall 2, Wall 3, Wall 4])
                      wallCat
        floorCandidates =
            concatMap (candidatesFor worst [Floor])
                      floorCat

        candidates = wallCandidates ++ floorCandidates
        valid = filter isValid candidates
    in case valid of
        [] -> Nothing
        _  ->
            let (d, s, a, _) =
                    maximumBy (comparing (\(_, _, _, dA) -> abs dA)) valid
            in Just (d, s, a)
  where
    candidatesFor worst sids decor =
        [ (decor, surfaceId surface, area, deltaA)
        | surface <- roomSurfaces room
        , surfaceId surface `elem` sids
        , let freeArea = calculateFreeArea surface (roomSeats room)
        , freeArea > 0
        , let d_f = deltaOnFrequency decor surface worst
        , d_f /= 0
        , let sReq = requiredArea acoustic purpose worst decor surface
        , let sLim = limitArea acoustic purpose decor surface
        , let area = minimum [freeArea, sReq, sLim]
        , area > 0
        , let deltaA = d_f * area
        , deltaA > 0
        ]

    isValid (_, _, area, _) = area > 0

data Change
    = AddDecor    SurfaceId DecorMaterial Double
    | RemoveDecor SurfaceId DecorMaterial
    deriving (Show, Eq)


applyChange :: Change -> Room -> Room
applyChange change room =
    case change of
        AddDecor sid decor area ->
            room { roomSurfaces = map (addDecor sid decor area) (roomSurfaces room) }
        RemoveDecor sid decor ->
            room { roomSurfaces = map (removeDecorFromSurface sid decor) (roomSurfaces room) }


addDecor :: SurfaceId -> DecorMaterial -> Double -> Surface -> Surface
addDecor sid decor area surface
    | surfaceId surface == sid =
        surface { surfaceElems =
                    surfaceElems surface ++ [ElemDecor (DecorItem decor area)] }
    | otherwise = surface


removeDecorFromSurface :: SurfaceId -> DecorMaterial -> Surface -> Surface
removeDecorFromSurface sid decor surface
    | surfaceId surface == sid =
        surface { surfaceElems =
                    filter (not . isDecor) (surfaceElems surface) }
    | otherwise = surface
  where
    isDecor (ElemDecor d) = decorName (decorMaterial d) == decorName decor
    isDecor _             = False


applyRecommendations :: [Recommendation] -> Room -> Room
applyRecommendations recs room =
    foldl (\r rec -> applyChange (toChange rec) r) room recs
  where
    toChange rec =
        case action rec of
            Add    -> AddDecor    (targetSurface rec) (targetElement rec) (recommendedArea rec)
            Remove -> RemoveDecor (targetSurface rec) (targetElement rec)

recalculateAcoustic :: Room -> AcousticResult
recalculateAcoustic room =
    let volume       = calculateVolume (roomDimensions room)
        commonArea'  = calculateCommonArea (roomDimensions room) (roomSeats room)
        absorption   = calculateTotalAbsorption
                          (roomSurfaces room)
                          (roomSeats room)
                          commonArea'
        reverb       = calculateReverberationSpectrum volume absorption commonArea'
    in AcousticResult
        { roomVolume        = volume
        , commonArea        = commonArea'
        , totalAbsorption   = absorption
        , reverberationTime = reverb
        }

buildRecommendation :: DecorMaterial -> SurfaceId -> Double -> AcousticResult -> AcousticResult
    -> SuitabilityResult -> Recommendation
buildRecommendation decor sid area oldAcoustic newAcoustic newSuitability =
    Recommendation
        { action          = Add
        , targetElement   = decor
        , targetSurface   = sid
        , recommendedArea = area
        , previousReverb  = reverberationTime oldAcoustic
        , newReverb       = reverberationTime newAcoustic
        , newSuitability  = newSuitability
        }