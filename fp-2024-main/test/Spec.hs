module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Lib2
import Lib3
import System.Directory (removeFile)
import Control.Exception (bracket_)

-- Arbitrary Instances for Property-Based Testing
instance Arbitrary Ingredient where
    arbitrary = Ingredient
        <$> listOf1 (choose ('a', 'z')) -- Random name
        <*> arbitrary                    -- Random quantity
        <*> arbitrary                    -- Random calorie value

instance Arbitrary Recipe where
    arbitrary = Recipe
        <$> listOf1 (choose ('a', 'z')) -- Random recipe name
        <*> arbitrary                    -- Random ingredients

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ propertyTests
    , unitTests
    ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
    [ QC.testProperty "Saving and then loading recipes preserves state" $
        \recipes -> ioProperty $ do
            let fileName = "test_recipes1.txt"
            bracket_ (return ()) (removeFile fileName) $ do
                saveRecipes fileName recipes
                loadedRecipes <- loadRecipes fileName
                return $ recipes == loadedRecipes

    , QC.testProperty "Saved queries reproduce original state" $
        \recipes queryName -> ioProperty $ do
            let fileName = "test_recipes2.txt"
            bracket_ (return ()) (removeFile fileName) $ do
                saveRecipes fileName recipes
                loadedRecipes <- loadRecipes fileName
                let originalResults = filter ((== queryName) . recipeName) recipes
                let loadedResults = filter ((== queryName) . recipeName) loadedRecipes
                return $ originalResults == loadedResults
    ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [ testCase "Serialize and deserialize an ingredient" $
        let ingredient = Ingredient "sugar" 100 400
            serialized = serializeIngredient ingredient
            deserialized = deserializeIngredient serialized
        in Just ingredient @=? deserialized

    , testCase "Serialize and deserialize a recipe" $
        let recipe = Recipe "cake" [Ingredient "sugar" 100 400, Ingredient "flour" 200 700]
            serialized = serializeRecipe recipe
            deserialized = deserializeRecipe serialized
        in Just recipe @=? deserialized
    ]
