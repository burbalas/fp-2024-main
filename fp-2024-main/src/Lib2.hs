module Lib2
    ( Command(..),
      AddCommand(..),
      SearchCommand(..),
      Ingredient(..),
      parseCommand
    ) where

import Data.Char (isAlpha, isDigit)

-- Command and related data types
data Command
    = Add AddCommand
    | Remove String
    | ListRecipes
    | Search SearchCommand
    | Exit
    deriving (Show, Eq)

data AddCommand
    = AddRecipe String [Ingredient]
    | AddSubRecipe String String
    | AddSubRecipes String [String]
    | AddSubRecipeWithIngredients String String [Ingredient]
    deriving (Show, Eq)

data SearchCommand
    = SearchByName String
    | SearchByIngredient String
    deriving (Show, Eq)

data Ingredient = Ingredient
    { ingredientName :: String
    , quantity :: Int
    , calorieValue :: Int
    }
    deriving (Show, Eq)

type Parser a = String -> Either String (a, String)

-- Helper parsers
dropWhitespace :: String -> String
dropWhitespace = dropWhile (`elem` " \t\n")

parseWord :: Parser String
parseWord input =
    let (word, rest) = span isAlpha (dropWhitespace input)
    in if null word
       then Left "Expected a word"
       else Right (word, rest)

parseDigits :: Parser Int
parseDigits input =
    let (digits, rest) = span isDigit (dropWhitespace input)
    in if null digits
       then Left "Expected a number"
       else Right (read digits, rest)

parseIngredient :: Parser Ingredient
parseIngredient input =
    case parseWord input of
        Left err -> Left err
        Right (name, rest1) ->
            case parseDigits rest1 of
                Left err -> Left err
                Right (qty, rest2) ->
                    case parseDigits rest2 of
                        Left err -> Left err
                        Right (cal, rest3) ->
                            Right (Ingredient name qty cal, rest3)

-- New function to parse subrecipes and ingredients dynamically
parseSubrecipesAndIngredients :: String -> Either String ([String], [Ingredient], String)
parseSubrecipesAndIngredients input =
    let trimmed = dropWhitespace input
    in parseLoop trimmed [] []

parseLoop :: String -> [String] -> [Ingredient] -> Either String ([String], [Ingredient], String)
parseLoop "" subRecipes ingredients = Right (subRecipes, ingredients, "")
parseLoop input subRecipes ingredients =
    case parseIngredient input of
        Right (ingredient, rest) -> parseLoop rest subRecipes (ingredients ++ [ingredient])
        Left _ -> -- If not an ingredient, check for subrecipe
            case parseWord input of
                Right (word, rest) ->
                    if all isAlpha word
                    then parseLoop rest (subRecipes ++ [word]) ingredients
                    else Left "Invalid input: expected subrecipe or ingredient"
                Left _ -> Right (subRecipes, ingredients, input)

-- Updated parseAddCommand
parseAddCommand :: Parser AddCommand
parseAddCommand input =
    case parseWord input of
        Left err -> Left err
        Right (recipeName, rest1) ->
            case parseSubrecipesAndIngredients rest1 of
                Right (subRecipes, ingredients, rest2) ->
                    if null subRecipes
                    then Right (AddRecipe recipeName ingredients, rest2)
                    else if null ingredients
                         then Right (AddSubRecipes recipeName subRecipes, rest2)
                         else Right (AddSubRecipeWithIngredients recipeName (unwords subRecipes) ingredients, rest2)
                Left err -> Left err

parseRemoveCommand :: Parser String
parseRemoveCommand = parseWord

parseSearchCommand :: Parser SearchCommand
parseSearchCommand input =
    case parseWord input of
        Left err -> Left err
        Right ("name", rest) ->
            case parseWord rest of
                Left err -> Left err
                Right (recipeName, rest') -> Right (SearchByName recipeName, rest')
        Right ("ingredient", rest) ->
            case parseWord rest of
                Left err -> Left err
                Right (ingredientName, rest') -> Right (SearchByIngredient ingredientName, rest')
        Right _ -> Left "Invalid search command"

parseCommand :: Parser Command
parseCommand input =
    let trimmed = dropWhitespace input
    in case parseWord trimmed of
        Right ("add", rest) ->
            case parseAddCommand rest of
                Left err -> Left err
                Right (addCmd, rest') -> Right (Add addCmd, rest')
        Right ("remove", rest) ->
            case parseRemoveCommand rest of
                Left err -> Left err
                Right (recipeName, rest') -> Right (Remove recipeName, rest')
        Right ("list", rest) ->
            if "recipes" `elem` words rest then Right (ListRecipes, "") else Left "Expected 'list recipes'"
        Right ("search", rest) ->
            case parseSearchCommand rest of
                Left err -> Left err
                Right (searchCmd, rest') -> Right (Search searchCmd, rest')
        Right ("exit", rest) -> Right (Exit, rest)
        Right _ -> Left "Unknown command"
        Left err -> Left err
