module Main where

import Lib2
import System.IO (hFlush, stdout)

data Recipe = Recipe
    { recipeName :: String
    , ingredients :: [Ingredient]
    } deriving (Show, Eq)

main :: IO ()
main = do
    repl []

repl :: [Recipe] -> IO ()
repl recipes = do
    putStr "Enter a command: "
    hFlush stdout
    input <- getLine
    case parseCommand input of
        Left err -> do
            putStrLn $ "Error: " ++ err
            repl recipes
        Right (command, _) -> do
            (continue, newRecipes) <- executeCommand command recipes
            if continue then repl newRecipes else return ()

executeCommand :: Command -> [Recipe] -> IO (Bool, [Recipe])
executeCommand (Add (AddRecipe name ingredients)) recipes =
    if any ((== name) . recipeName) recipes
    then do
        putStrLn "Recipe already exists."
        return (True, recipes)
    else do
        putStrLn $ "Added recipe: " ++ name
        return (True, Recipe name ingredients : recipes)

executeCommand (Add (AddSubRecipes name subRecipes)) recipes =
    let subRecipeIngredients = concatMap (\sub -> case findRecipe recipes sub of
                                                    Just (Recipe _ ing) -> ing
                                                    Nothing -> []) subRecipes
    in if null subRecipeIngredients
       then do
           putStrLn "One or more subrecipes not found."
           return (True, recipes)
       else do
           putStrLn $ "Added recipe: " ++ name ++ " with subrecipes: " ++ unwords subRecipes
           return (True, Recipe name subRecipeIngredients : recipes)

executeCommand (Add (AddSubRecipeWithIngredients name subRecipes ingredients)) recipes =
    let subRecipeIngredients = concatMap (\sub -> case findRecipe recipes sub of
                                                    Just (Recipe _ ing) -> ing
                                                    Nothing -> []) (words subRecipes)
        combinedIngredients = subRecipeIngredients ++ ingredients
    in if null subRecipeIngredients
       then do
           putStrLn "One or more subrecipes not found."
           return (True, recipes)
       else do
           putStrLn $ "Added recipe: " ++ name ++ " with subrecipes: " ++ subRecipes
                     ++ " and additional ingredients."
           return (True, Recipe name combinedIngredients : recipes)

executeCommand (Remove name) recipes =
    let newRecipes = filter ((/= name) . recipeName) recipes
    in do
        putStrLn $ "Removed recipe: " ++ name
        return (True, newRecipes)

executeCommand ListRecipes recipes = do
    if null recipes
    then putStrLn "No recipes found."
    else mapM_ (putStrLn . recipeName) recipes
    return (True, recipes)

executeCommand (Search (SearchByName name)) recipes = do
    case findRecipe recipes name of
        Just recipe -> do
            putStrLn $ "Recipe: " ++ recipeName recipe
            mapM_ (putStrLn . showIngredient) (ingredients recipe)
        Nothing -> putStrLn $ "Recipe " ++ name ++ " not found."
    return (True, recipes)

executeCommand (Search (SearchByIngredient ingredient)) recipes = do
    let results = filter (hasIngredient ingredient) recipes
    if null results
    then putStrLn $ "No recipes found with ingredient: " ++ ingredient
    else do
        putStrLn $ "Recipes with " ++ ingredient ++ ":"
        mapM_ (putStrLn . recipeName) results
    return (True, recipes)

executeCommand Exit recipes = do
    putStrLn "Exiting the program."
    return (False, recipes)

findRecipe :: [Recipe] -> String -> Maybe Recipe
findRecipe recipes name = case filter ((== name) . recipeName) recipes of
    [] -> Nothing
    (x:_) -> Just x

hasIngredient :: String -> Recipe -> Bool
hasIngredient ingredient recipe =
    any ((== ingredient) . ingredientName) (ingredients recipe)

showIngredient :: Ingredient -> String
showIngredient (Ingredient name qty cal) =
    name ++ " (Mass: " ++ show qty ++ ", Calories: " ++ show cal ++ ")"