{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib2
import Lib3
import Control.Concurrent.STM
import Control.Monad (unless, foldM)
import System.Directory (doesFileExist)
import System.IO

data REPLState = Normal | Grouping String [ExtendedCommand]

main :: IO ()
main = do
    recipesVar <- newTVarIO []
    repl Normal recipesVar

repl :: REPLState -> TVar [Recipe] -> IO ()
repl state recipesVar = do
    putStr "Enter a command: "
    hFlush stdout
    input <- getLine
    case parseExtendedCommand input of
        Left err -> do
            putStrLn $ "Error: " ++ err
            repl state recipesVar
        Right command -> handleCommand state command recipesVar

handleCommand :: REPLState -> ExtendedCommand -> TVar [Recipe] -> IO ()
handleCommand Normal (BatchFile fileName) recipesVar = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- readFile fileName
            result <- atomically $ batchProcessAtomic contents recipesVar
            case result of
                Left err -> putStrLn $ "Batch processing failed: " ++ err
                Right logs -> mapM_ putStrLn logs
            repl Normal recipesVar
        else do
            putStrLn $ "File not found: " ++ fileName
            repl Normal recipesVar

handleCommand Normal (Command cmd) recipesVar = do
    result <- atomically $ processSingleCommand recipesVar cmd
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right logs -> mapM_ putStrLn logs
    unless (cmd == Exit) $ repl Normal recipesVar

handleCommand Normal Help recipesVar = do
    showHelp
    repl Normal recipesVar

handleCommand Normal (Save fileName) recipesVar = do
    recipes <- readTVarIO recipesVar
    saveRecipes fileName recipes
    putStrLn $ "Recipes saved to file: " ++ fileName
    repl Normal recipesVar

handleCommand Normal (Load fileName) recipesVar = do
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            loadedRecipes <- loadRecipes fileName
            atomically $ writeTVar recipesVar loadedRecipes
            putStrLn $ "Recipes loaded from file: " ++ fileName
            repl Normal recipesVar
        else do
            putStrLn $ "File not found: " ++ fileName
            repl Normal recipesVar
   

handleCommand Normal (Begin groupName) recipesVar = do
    putStrLn $ "Started grouping session: " ++ groupName
    repl (Grouping groupName []) recipesVar

handleCommand (Grouping groupName commands) End recipesVar = do
    result <- atomically $ executeGroupedCommands recipesVar commands
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right logs -> mapM_ putStrLn logs
    repl Normal recipesVar

handleCommand (Grouping groupName commands) cmd recipesVar = do
    putStrLn $ "Added command to group: " ++ show cmd
    repl (Grouping groupName (commands ++ [cmd])) recipesVar

handleCommand _ _ _ = putStrLn "Command not recognized."

processSingleCommand :: TVar [Recipe] -> Command -> STM (Either String [String])
processSingleCommand recipesVar cmd = do
    recipes <- readTVar recipesVar
    case executeCommandSync cmd recipes of
        Left err -> return $ Left err
        Right (newRecipes, logs) -> do
            writeTVar recipesVar newRecipes
            return $ Right logs

batchProcessAtomic :: String -> TVar [Recipe] -> STM (Either String [String])
batchProcessAtomic input recipesVar = do
    recipes <- readTVar recipesVar
    let parsedCommands = traverse parseExtendedCommand (lines input)
    case parsedCommands of
        Left err -> return $ Left $ "Parsing error: " ++ err
        Right commands -> do
            let invalidCommands = filter isInvalidCommand commands
            if not (null invalidCommands)
                then return $ Left "Error: Save and Load commands are not allowed in batch mode."
                else executeGroupedCommands recipesVar commands

isInvalidCommand :: ExtendedCommand -> Bool
isInvalidCommand (Save _) = True
isInvalidCommand (Load _) = True
isInvalidCommand _ = False


executeGroupedCommands :: TVar [Recipe] -> [ExtendedCommand] -> STM (Either String [String])
executeGroupedCommands recipesVar commands = do
    recipes <- readTVar recipesVar
    let groupedCommands = [cmd | Command cmd <- commands]
    case processCommandsAtomic groupedCommands recipes of
        Left err -> return $ Left err
        Right (newRecipes, logs) -> do
            writeTVar recipesVar newRecipes
            return $ Right logs

processCommandsAtomic :: [Command] -> [Recipe] -> Either String ([Recipe], [String])
processCommandsAtomic commands recipes =
    foldM applyCommand (recipes, []) commands
  where
    applyCommand :: ([Recipe], [String]) -> Command -> Either String ([Recipe], [String])
    applyCommand (currentRecipes, logs) cmd =
        case executeCommandSync cmd currentRecipes of
            Left err -> Left err
            Right (newRecipes, logOutput) -> Right (newRecipes, logs ++ logOutput)




executeCommandSync :: Command -> [Recipe] -> Either String ([Recipe], [String])
executeCommandSync (Add (AddRecipe name ingredients)) recipes =
    if any ((== name) . recipeName) recipes
    then Left $ "Recipe " ++ name ++ " already exists."
    else Right (Recipe name ingredients : recipes, ["Added recipe: " ++ name])

executeCommandSync (Add (AddSubRecipes name subRecipes)) recipes =
    let missingSubRecipes = filter (\sub -> findRecipe recipes sub == Nothing) subRecipes
    in if not (null missingSubRecipes)
       then Left $ "One or more sub-recipes not found: " ++ unwords missingSubRecipes
       else
           let subRecipeIngredients = concatMap (\sub -> case findRecipe recipes sub of
                                                           Just (Recipe _ ing) -> ing
                                                           Nothing -> []) subRecipes
           in Right (Recipe name subRecipeIngredients : recipes,
                     ["Added recipe: " ++ name ++ " with sub-recipes: " ++ unwords subRecipes])


executeCommandSync (Add (AddSubRecipeWithIngredients name subRecipes ingredients)) recipes =
    let subRecipeList = words subRecipes
        missingSubRecipes = filter (\sub -> findRecipe recipes sub == Nothing) subRecipeList
    in if not (null missingSubRecipes)
       then Left $ "One or more sub-recipes not found: " ++ unwords missingSubRecipes
       else
           let subRecipeIngredients = concatMap (\sub -> case findRecipe recipes sub of
                                                           Just (Recipe _ ing) -> ing
                                                           Nothing -> []) subRecipeList
               combinedIngredients = subRecipeIngredients ++ ingredients
           in Right (Recipe name combinedIngredients : recipes,
                     ["Added recipe: " ++ name ++ " with sub-recipes and additional ingredients."])



executeCommandSync (Remove name) recipes =
    if any ((== name) . recipeName) recipes
    then
        let newRecipes = filter ((/= name) . recipeName) recipes
        in Right (newRecipes, ["Removed recipe: " ++ name])
    else
        Left $ "Recipe " ++ name ++ " does not exist."

executeCommandSync ListRecipes recipes =
    let logOutput = if null recipes
                    then ["No recipes found."]
                    else "Recipes:" : map recipeName recipes
    in Right (recipes, logOutput)

executeCommandSync (Search (SearchByName name)) recipes =
    case findRecipe recipes name of
        Just (Recipe recipeName ingredients) ->
            Right (recipes, [recipeName ++ ":\n" ++ unlines (map showIngredient ingredients)])
        Nothing -> Left $ "Recipe " ++ name ++ " not found."

executeCommandSync (Search (SearchByIngredient ingredient)) recipes =
    let results = filter (hasIngredient ingredient) recipes
        logOutput = if null results
                    then ["No recipes found with ingredient: " ++ ingredient]
                    else ["Recipes with " ++ ingredient ++ ":\n" ++ unlines (map recipeName results)]
    in Right (recipes, logOutput)


executeCommandSync _ recipes =
    Right (recipes, ["Command executed successfully."])


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
    name ++ " (Quantity: " ++ show qty ++ ", Calories: " ++ show cal ++ ")"


    