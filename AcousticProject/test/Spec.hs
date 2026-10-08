module Main (main) where

import Test.QuickCheck

import qualified Calculation.GeometrySpec
import qualified Calculation.ReverberationSpec
import qualified Types.SpectrumSpec
import qualified Evaluation.SuitabilitySpec


main :: IO ()
main = do
    putStrLn "=== Calculation.Geometry ==="
    quickCheck Calculation.GeometrySpec.prop_volumeNonNegative
    quickCheck Calculation.GeometrySpec.prop_volumeFormula
    quickCheck Calculation.GeometrySpec.prop_totalSurfaceAreaGreaterThanFloor

    putStrLn "=== Calculation.Reverberation ==="
    quickCheck Calculation.ReverberationSpec.prop_sabineNonNegative
    quickCheck Calculation.ReverberationSpec.prop_sabineZeroWhenAbsorptionZero
    quickCheck Calculation.ReverberationSpec.prop_sabineFormula
    quickCheck Calculation.ReverberationSpec.prop_sabineProportionalToVolume

    putStrLn "--- Эйринг ---"
    quickCheck Calculation.ReverberationSpec.prop_eyringNonNegative
    quickCheck Calculation.ReverberationSpec.prop_eyringZeroWhenCommonAreaZero
    quickCheck Calculation.ReverberationSpec.prop_eyringZeroWhenPhiZero
    quickCheck Calculation.ReverberationSpec.prop_eyringFormula
    quickCheck Calculation.ReverberationSpec.prop_eyringProportionalToVolume
    quickCheck Calculation.ReverberationSpec.prop_eyringInverselyProportionalToCommonArea
    quickCheck Calculation.ReverberationSpec.prop_eyringInverselyProportionalToPhi

    putStrLn "=== Types.Spectrum ==="
    quickCheck Types.SpectrumSpec.prop_addSpectrumsCommutative
    quickCheck Types.SpectrumSpec.prop_addSpectrumsIdentity
    quickCheck Types.SpectrumSpec.prop_multiplySpectrumByOne
    quickCheck Types.SpectrumSpec.prop_subtractSpectrumsSelf

    putStrLn "=== Evaluation.Suitability ==="
    quickCheck Evaluation.SuitabilitySpec.prop_deviationNonNegative
    quickCheck Evaluation.SuitabilitySpec.prop_deviationZeroInRange
    quickCheck Evaluation.SuitabilitySpec.prop_gradeSuitableWhenZero