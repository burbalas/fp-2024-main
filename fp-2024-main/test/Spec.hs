{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Lib2
import Lib3
import Control.DeepSeq (NFData, force)
import GHC.Generics (Generic)

-- Add Generic and NFData instances for Ingredient and Recipe
deriving instance Generic Ingredient
deriving instance NFData Ingredient

deriving instance Generic Recipe
deriving instance NFData Recipe

-- Arbitrary Instances for Property-Based Testing
instance Arbitrary Ingredient where
    arbitrary = Ingredient
        <$> listOf1 (choose ('a', 'z')) 
        <*> arbitrary                    
        <*> arbitrary                    

instance Arbitrary Recipe where
    arbitrary = Recipe
        <$> listOf1 (choose ('a', 'z')) 
        <*> arbitrary                   

main :: IO ()
main = defaultMain tests

tests = testGroup "Parsing Tests"
    [ testCase "parses a single word correctly" $
        parseWord "  Hello World" @?= Right ("Hello", " World")

    , testCase "returns an error if no word is found" $
        parseWord "  12345" @?= Left "Expected a word"

    , testCase "parses an ingredient correctly" $
        parseIngredient "flour 200 100" @?= Right (Ingredient "flour" 200 100, "")

    , testCase "returns an error if ingredient is incomplete" $
        parseIngredient "flour 200" @?= Left "Expected a number"

    , testCase "parses add command with ingredients" $
        parseAddCommand "Breakfast eggs 200 300" @?= Right (AddRecipe "Breakfast" [Ingredient "eggs" 200 300], "")

    , testCase "parses add command with subrecipes" $
        parseAddCommand "Breakfast Pancakes" @?= Right (AddSubRecipes "Breakfast" ["Pancakes"], "")

    , testCase "parses add command with both subrecipes and ingredients" $
        parseAddCommand "Breakfast Pancakes eggs 200 300" @?=
            Right (AddSubRecipeWithIngredients "Breakfast" "Pancakes" [Ingredient "eggs" 200 300], "")

    , testCase "returns an error for an invalid add command" $
        parseAddCommand "Breakfast" @?= Right (AddRecipe "Breakfast" [], "")

    , testCase "parses a valid search command by name" $
        parseSearchCommand "name Breakfast" @?= Right (SearchByName "Breakfast", "")

    , testCase "parses a valid search command by ingredient" $
        parseSearchCommand "ingredient eggs" @?= Right (SearchByIngredient "eggs", "")

    , testCase "returns an error for an invalid search command" $
        parseSearchCommand "random_command" @?= Left "Invalid search command"

    , testCase "parses a valid command to remove a recipe" $
        parseCommand "remove Breakfast" @?= Right (Remove "Breakfast", "")

    , testCase "parses a valid command to list recipes" $
        parseCommand "list recipes" @?= Right (ListRecipes, "")

    , testCase "parses a valid command to exit" $
        parseCommand "exit" @?= Right (Exit, "")

    , testCase "returns an error for an invalid command" $
        parseCommand "random_command" @?= Left "Unknown command"
    ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
    [ QC.testProperty "Saving and then loading recipes preserves state" $
        \recipes -> ioProperty $ do
            let fileName = "test_recipes1.txt"
            saveRecipes fileName recipes
            loadedRecipes <- loadRecipes fileName
            
            return $ recipes == force loadedRecipes

    , QC.testProperty "Saved queries reproduce original state" $
        \recipes queryName -> ioProperty $ do
            let fileName = "test_recipes2.txt"
            saveRecipes fileName recipes
            loadedRecipes <- loadRecipes fileName
            
            let originalResults = filter ((== queryName) . recipeName) recipes
            let loadedResults = filter ((== queryName) . recipeName) (force loadedRecipes)
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
