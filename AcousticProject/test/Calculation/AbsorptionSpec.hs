module Calculation.AbsorptionSpec where

import Test.QuickCheck

import Types.Material
import Types.Room
import Types.Geometry
import Calculation.Absorption


materialGenerator :: Gen Material
materialGenerator = do
    name <- elements
        [ "Бетон"
        , "Ковролин"
        , "Шторы"
        , "Дерево"
        ]

    coefficient <- choose (0, 1)
    removableValue <- arbitrary

    pure $
        Material
            { materialName = name
            , absorptionCoefficient = coefficient
            , removable = removableValue
            }


surfaceGenerator :: Gen Surface
surfaceGenerator = do
    material <- materialGenerator
    area <- choose (0, 100)

    pure $
        Surface
            { surfaceMaterial = material
            , surfaceArea = area
            }


itemGenerator :: Gen AdjustableItem
itemGenerator = do
    material <- materialGenerator
    area <- choose (0, 100)
    name <- elements
        [ "Ковролин"
        , "Шторы"
        , "Мебель"
        ]

    pure $
        AdjustableItem
            { itemName = name
            , itemMaterial = material
            , itemArea = area
            }


prop_surfaceAbsorptionNonNegative :: Property
prop_surfaceAbsorptionNonNegative =
    forAll surfaceGenerator $ \surface ->
        calculateSurfaceAbsorption surface >= 0


prop_surfaceAbsorptionFormula :: Property
prop_surfaceAbsorptionFormula =
    forAll surfaceGenerator $ \surface ->
        let material = surfaceMaterial surface

            expected =
                absorptionCoefficient material
                    * surfaceArea surface

        in abs
            (calculateSurfaceAbsorption surface - expected)
            < 1e-9


prop_itemAbsorptionNonNegative :: Property
prop_itemAbsorptionNonNegative =
    forAll itemGenerator $ \item ->
        calculateItemAbsorption item >= 0


prop_itemAbsorptionFormula :: Property
prop_itemAbsorptionFormula =
    forAll itemGenerator $ \item ->
        let material = itemMaterial item

            expected =
                absorptionCoefficient material
                    * itemArea item

        in abs
            (calculateItemAbsorption item - expected)
            < 1e-9


prop_roomAbsorptionNonNegative :: Positive Double -> Bool
prop_roomAbsorptionNonNegative (Positive area) =
    let material =
            Material
                { materialName = "Тест"
                , absorptionCoefficient = 0.5
                , removable = True
                }

        room =
            Room
                { roomName = "Тестовая комната"
                , dimensions = Dimensions 5 4 3
                , surfaces =
                    [ Surface material area
                    ]
                , adjustableItems = []
                }

    in calculateAbsorptionArea room >= 0