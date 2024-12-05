module Lib2
    ( Command(..),
      AddCommand(..),
      SearchCommand(..),
      Ingredient(..),
      parseCommand,
      parseWord,
      parseIngredient,
      parseAddCommand,
      parseSearchCommand
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

dropWhitespace :: String -> String
dropWhitespace = dropWhile (`elem` " \t\n")

-- <command> ::= "add" " " <add_command> 
--              | "remove" " " <remove_command> 
--              | "list recipes"
--              | "search" " " <search_command> 
--              | "exit"
parseCommand :: String -> Either String (Command, String)
parseCommand input =
    case parseWord (dropWhitespace input) of
        Right ("add", rest) -> fmap (\(addCmd, rest') -> (Add addCmd, rest')) (parseAddCommand rest)
        Right ("remove", rest) -> fmap (\(name, rest') -> (Remove name, rest')) (parseWord rest)
        Right ("list", _) -> Right (ListRecipes, "")
        Right ("search", rest) ->
            fmap (\(searchCmd, rest') -> (Search searchCmd, rest')) (parseSearchCommand rest)
        Right ("exit", _) -> Right (Exit, "")
        Right _ -> Left "Unknown command"
        Left err -> Left err

-- <add_command> ::= <recipe_name> " " <ingredients> 
--                | <recipe_name> " " <recipe> 
parseAddCommand :: String -> Either String (AddCommand, String)
parseAddCommand input =
    case parseWord input of
        Left err -> Left err
        Right (recipeName, rest1) ->
            let trimmed = dropWhitespace rest1
            in if null trimmed
               then Right (AddRecipe recipeName [], "")
               else case parseSubrecipesAndIngredients trimmed of
                   Right (subRecipes, ingredients, rest2) ->
                       if null subRecipes
                       then Right (AddRecipe recipeName ingredients, rest2)
                       else if null ingredients
                            then Right (AddSubRecipes recipeName subRecipes, rest2)
                            else Right (AddSubRecipeWithIngredients recipeName (unwords subRecipes) ingredients, rest2)
                   Left err -> Left err


-- <search_command> ::= "name" " " <recipe_name> 
--                   | "ingredient" " " <ingredient_name>
parseSearchCommand :: String -> Either String (SearchCommand, String)
parseSearchCommand input =
    case parseWord input of
        Right ("name", rest) -> fmap (\(name, rest') -> (SearchByName name, rest')) (parseWord rest)
        Right ("ingredient", rest) -> fmap (\(name, rest') -> (SearchByIngredient name, rest')) (parseWord rest)
        Right _ -> Left "Invalid search command"
        Left err -> Left err


-- <recipe_name> ::= <word> | <word> <recipe_name>
-- <ingredient_name> ::= <word> | <word> <ingredient_name>
parseWord :: String -> Either String (String, String)
parseWord input =
    let (word, rest) = span isAlpha (dropWhitespace input)
    in if null word
       then Left "Expected a word"
       else Right (word, rest)

parseDigits :: String -> Either String (Int, String)
parseDigits input =
    let (digits, rest) = span isDigit (dropWhitespace input)
    in if null digits
       then Left "Expected a number"
       else Right (read digits, rest)


-- <ingredient> ::= <ingredient_name> <quantity> <calorie_value>
-- <quantity> ::= <digit> | <digit> <quantity>
-- <calorie_value> ::= <digit> | <digit> <calorie_value>
parseIngredient :: String -> Either String (Ingredient, String)
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


-- <ingredients> ::= <ingredient> | <ingredient> <ingredients>
-- <recipe> ::= <ingredients> | <sub_recipe>
--             | <ingredients> <sub_recipe>
--             | <sub_recipe> <ingredients> 
--             | <sub_recipe> <recipe>
-- <sub_recipe> ::= <recipe_name>

parseSubrecipesAndIngredients :: String -> Either String ([String], [Ingredient], String)
parseSubrecipesAndIngredients input = parseLoop (dropWhitespace input) [] []


parseLoop :: String -> [String] -> [Ingredient] -> Either String ([String], [Ingredient], String)
parseLoop "" subRecipes ingredients = Right (subRecipes, ingredients, "")
parseLoop input subRecipes ingredients =
    case parseIngredient input of
        Right (ingredient, rest) ->
            parseLoop rest subRecipes (ingredients ++ [ingredient])
        Left _ ->
            case parseWord input of
                Right (word, rest) ->
                    if all isAlpha word
                    then parseLoop rest (subRecipes ++ [word]) ingredients
                    else Left "Invalid input: expected subrecipe or ingredient"
                Left _ -> Right (subRecipes, ingredients, input)



parseIngredientList :: String -> ([Ingredient], String)
parseIngredientList input =
    let trimmed = dropWhitespace input
    in case parseIngredient trimmed of
        Right (ingredient, rest) ->
            let (nextIngredients, remaining) = parseIngredientList rest
            in (ingredient : nextIngredients, remaining)
        Left _ -> ([], trimmed) 


parseSubrecipes :: String -> Either String ([String], String)
parseSubrecipes input =
    let trimmed = dropWhitespace input
    in parseSubrecipeLoop trimmed []

parseSubrecipeLoop :: String -> [String] -> Either String ([String], String)
parseSubrecipeLoop input subRecipes =
    let trimmed = dropWhitespace input
    in case parseWord trimmed of
        Right (word, rest) ->
            if all isAlpha word
            then parseSubrecipeLoop rest (subRecipes ++ [word])
            else Right (subRecipes, trimmed)
        Left _ -> Right (subRecipes, trimmed)