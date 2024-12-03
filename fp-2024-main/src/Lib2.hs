module Lib2
    ( Command(..),
      AddCommand(..),
      SearchCommand(..),
      Ingredient(..),
      parseCommand
    ) where

import Data.Char (isAlpha, isDigit)


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


parseIngredients :: Parser [Ingredient]
parseIngredients input =
    case parseIngredient input of
        Left err -> Left err
        Right (ingredient, rest) ->
            case parseIngredients rest of
                Left _ -> Right ([ingredient], rest) 
                Right (ingredients, rest') -> Right (ingredient : ingredients, rest')


parseAddCommand :: Parser AddCommand
parseAddCommand input =
    case parseWord input of
        Left err -> Left err
        Right (recipeName, rest1) ->
            
            case parseWord rest1 of
                Right (potentialSubRecipe, rest2) ->
                    case parseIngredients rest2 of
                        
                        Right (ingredients, rest3) ->
                            Right (AddSubRecipeWithIngredients recipeName potentialSubRecipe ingredients, rest3)
                        
                        Left _ ->
                            case parseIngredients rest1 of
                                Right (ingredients, rest4) ->
                                    Right (AddRecipe recipeName ingredients, rest4)
                                Left err -> Left err
                
                Left _ ->
                    case parseIngredients rest1 of
                        Right (ingredients, rest2) ->
                            Right (AddRecipe recipeName ingredients, rest2)
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
