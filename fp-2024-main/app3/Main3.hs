module Main where

import Lib2
import Lib3
import Control.Monad (foldM)
import System.Directory (doesFileExist)
import System.IO

data REPLState = Normal | Grouping String [ExtendedCommand]

main :: IO ()
main = do
    repl Normal []

repl :: REPLState -> [Recipe] -> IO ()
repl state recipes = do
    putStr "Enter a command: "
    hFlush stdout
    input <- getLine
    case parseExtendedCommand input of
        Left err -> do
            putStrLn $ "Error: " ++ err
            repl state recipes
        Right command -> handleCommand state command recipes

handleCommand :: REPLState -> ExtendedCommand -> [Recipe] -> IO ()
handleCommand Normal (Command cmd) recipes = do
    (continue, newRecipes) <- executeCommand cmd recipes
    if continue
        then repl Normal newRecipes
        else putStrLn "Goodbye!"

handleCommand Normal (BatchFile fileName) recipes = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- readFile fileName
            (continue, newRecipes, errors) <- batchProcessCommands executeCommand contents recipes
            mapM_ putStrLn errors
            if continue
                then repl Normal newRecipes
                else putStrLn "Exiting program."
        else do
            putStrLn $ "File not found: " ++ fileName
            repl Normal recipes




handleCommand Normal (Save fileName) recipes = do
    saveRecipes fileName recipes
    putStrLn $ "Recipes saved to file: " ++ fileName
    repl Normal recipes

handleCommand Normal (Begin groupName) recipes = do
    putStrLn $ "Started grouping session: " ++ groupName
    repl (Grouping groupName []) recipes


handleCommand (Grouping groupName commands) End recipes = do
    putStrLn $ "Ending grouping session: " ++ groupName
    (newRecipes, errors) <- processGroupedCommands commands recipes
    mapM_ putStrLn errors
    repl Normal newRecipes


handleCommand (Grouping groupName commands) cmd recipes = do
    putStrLn $ "Adding command to group: " ++ show cmd
    repl (Grouping groupName (commands ++ [cmd])) recipes

handleCommand Normal Help recipes = do
    showHelp
    repl Normal recipes


handleCommand (Grouping groupName commands) End recipes = do
    putStrLn $ "Ending grouping session: " ++ groupName
    (newRecipes, errors) <- processGroupedCommands commands recipes
    mapM_ putStrLn errors
    repl Normal newRecipes

handleCommand (Grouping groupName commands) cmd recipes = do
    putStrLn $ "Adding command to group: " ++ show cmd
    repl (Grouping groupName (commands ++ [cmd])) recipes

    

handleCommand _ _ recipes = do
    putStrLn "Command not recognized."
    repl Normal recipes


processGroupedCommands :: [ExtendedCommand] -> [Recipe] -> IO ([Recipe], [String])
processGroupedCommands commands recipes = foldM executeGroupedCommand (recipes, []) commands


executeGroupedCommand :: ([Recipe], [String]) -> ExtendedCommand -> IO ([Recipe], [String])
executeGroupedCommand (currentRecipes, errors) (Command cmd) = do
    (continue, newRecipes) <- executeCommand cmd currentRecipes
    if continue
        then return (newRecipes, errors)
        else return (currentRecipes, errors ++ ["Batch processing interrupted."])
executeGroupedCommand result _ = return result



executeCommandSync :: Command -> [Recipe] -> Either String [Recipe]
executeCommandSync (Add (AddRecipe name ingredients)) recipes =
    if any ((== name) . recipeName) recipes
    then Left $ "Recipe " ++ name ++ " already exists."
    else Right (Recipe name ingredients : recipes)

executeCommandSync (Remove name) recipes =
    Right $ filter ((/= name) . recipeName) recipes

executeCommandSync ListRecipes recipes = Right recipes
executeCommandSync _ recipes = Right recipes

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


showHelp :: IO ()
showHelp = do
    putStrLn "Commands:"
    putStrLn "  add <recipe> <ingredients>"
    putStrLn "  add <recipe> <subrecipe>"
    putStrLn "  add <recipe> <subrecipes> <ingredients>"
    putStrLn "  remove <recipe>"
    putStrLn "  list recipes"
    putStrLn "  search <name|ingredient>"
    putStrLn "  batch - process batch commands"
    putStrLn "  save <filename> - save recipes to a file"
    putStrLn "  load <filename> - load recipes from a file"
    putStrLn "  begin <session> - start a grouping session"
    putStrLn "  end - end the current grouping session"
    putStrLn "  exit - exit the program"

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