{-# LANGUAGE ImportQualifiedPost #-}


import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Lib2

main :: IO ()
main = defaultMain tests

tests :: TestTree
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
