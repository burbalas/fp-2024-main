module Main where

import Lib2
import Control.Monad.State
import Control.Monad (when)
import Data.List (find)
import System.IO (hFlush, stdout)

type RecipeManager = StateT [Recipe] IO

data Recipe = Recipe
    { recipeName :: String
    , ingredients :: [Ingredient]
    } deriving (Show, Eq)

main :: IO ()
main = do
    putStrLn "Welcome to the Recipe Manager!"
    evalStateT repl []

repl :: RecipeManager ()
repl = do
    liftIO $ putStr "Enter a command: "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case parseCommand input of
        Left err -> do
            liftIO $ putStrLn $ "Error: " ++ err
            repl
        Right (command, _) -> do
            continue <- executeCommand command
            when continue repl

executeCommand :: Command -> RecipeManager Bool
executeCommand (Add (AddRecipe name ingredients)) = do
    modify (addRecipe name ingredients)
    liftIO $ putStrLn $ "Added recipe: " ++ name
    return True
executeCommand (Add (AddSubRecipes name subRecipes)) = do
    recipes <- get
    let subRecipeIngredients = concatMap (\sub -> case findRecipe recipes sub of
                                                    Just (Recipe _ ing) -> ing
                                                    Nothing -> []) subRecipes
    if null subRecipeIngredients
        then liftIO $ putStrLn "One or more subrecipes not found."
        else do
            modify (addRecipe name subRecipeIngredients)
            liftIO $ putStrLn $ "Added recipe: " ++ name ++ " with subrecipes: " ++ unwords subRecipes
    return True
executeCommand (Add (AddSubRecipeWithIngredients name subRecipes ingredients)) = do
    recipes <- get
    let subRecipeIngredients = concatMap (\sub -> case findRecipe recipes sub of
                                                    Just (Recipe _ ing) -> ing
                                                    Nothing -> []) (words subRecipes)
    if null subRecipeIngredients
        then liftIO $ putStrLn "One or more subrecipes not found."
        else do
            let combinedIngredients = subRecipeIngredients ++ ingredients
            modify (addRecipe name combinedIngredients)
            liftIO $ putStrLn $ "Added recipe: " ++ name ++ " with subrecipes: " ++ subRecipes ++ " and additional ingredients."
    return True
executeCommand (Remove name) = do
    modify (filter ((/= name) . recipeName))
    liftIO $ putStrLn $ "Removed recipe: " ++ name
    return True
executeCommand ListRecipes = do
    recipes <- get
    if null recipes
        then liftIO $ putStrLn "No recipes found."
        else liftIO $ putStrLn $ "Recipes:\n" ++ unlines (map recipeName recipes)
    return True
executeCommand (Search (SearchByName name)) = do
    recipes <- get
    case findRecipe recipes name of
        Just recipe -> do
            liftIO $ putStrLn $ "Recipe: " ++ recipeName recipe
            liftIO $ mapM_ (liftIO . putStrLn . showIngredient) (ingredients recipe)
        Nothing -> liftIO $ putStrLn $ "Recipe " ++ name ++ " not found."
    return True
executeCommand (Search (SearchByIngredient ingredient)) = do
    recipes <- get
    let results = filter (hasIngredient ingredient) recipes
    if null results
        then liftIO $ putStrLn $ "No recipes found with ingredient: " ++ ingredient
        else do
            liftIO $ putStrLn $ "Recipes with " ++ ingredient ++ ":"
            liftIO $ mapM_ (liftIO . putStrLn . recipeName) results
    return True
executeCommand Exit = do
    liftIO $ putStrLn "Exiting the program."
    return False



addRecipe :: String -> [Ingredient] -> [Recipe] -> [Recipe]
addRecipe name ing recipes
    | any ((== name) . recipeName) recipes = recipes -- No duplicate added
    | otherwise = Recipe name ing : recipes

findRecipe :: [Recipe] -> String -> Maybe Recipe
findRecipe recipes name = find ((== name) . recipeName) recipes

hasIngredient :: String -> Recipe -> Bool
hasIngredient ingredient recipe =
    any ((== ingredient) . ingredientName) (ingredients recipe)

showIngredient :: Ingredient -> String
showIngredient (Ingredient name qty cal) =
    name ++ " (Quantity: " ++ show qty ++ ", Calories: " ++ show cal ++ ")"
