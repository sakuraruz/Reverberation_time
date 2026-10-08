module Types.SpectrumSpec where

import Test.QuickCheck

import Types.Spectrum


instance Arbitrary Spectrum where
    arbitrary = do
        a125  <- choose (-100.0, 100.0)
        a500  <- choose (-100.0, 100.0)
        a2000 <- choose (-100.0, 100.0)
        pure (Spectrum a125 a500 a2000)


prop_addSpectrumsCommutative :: Spectrum -> Spectrum -> Bool
prop_addSpectrumsCommutative a b =
    addSpectrums a b == addSpectrums b a


prop_addSpectrumsIdentity :: Spectrum -> Bool
prop_addSpectrumsIdentity a =
    addSpectrums a (Spectrum 0 0 0) == a


prop_multiplySpectrumByOne :: Spectrum -> Bool
prop_multiplySpectrumByOne a =
    multiplySpectrum a 1 == a


prop_subtractSpectrumsSelf :: Spectrum -> Bool
prop_subtractSpectrumsSelf a =
    subtractSpectrums a a == Spectrum 0 0 0