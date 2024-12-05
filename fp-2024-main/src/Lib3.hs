module Lib3
    ( Recipe(..)
    , ExtendedCommand(..)
    , batchProcessCommands
    , saveRecipes
    , loadRecipes
    , parseExtendedCommand
    , serializeIngredient
    , deserializeIngredient
    , serializeRecipe
    , deserializeRecipe
    ) where

import Lib2
import System.Directory (doesFileExist)
import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import System.IO


data Recipe = Recipe
    { recipeName :: String
    , ingredients :: [Ingredient]
    } deriving (Show, Eq)


data ExtendedCommand
    = Command Command               
    | Save String                    
    | Begin String                  
    | Load String                    
    | End                            
    | Help                          
    | BatchFile String               
    deriving (Show, Eq)



parseExtendedCommand :: String -> Either String ExtendedCommand
parseExtendedCommand input =
    case parseCommand input of
        Right (command, _) -> Right (Command command)
        Left _ -> case words input of
            ("save" : fileName : _) -> Right (Save fileName)
            ("begin" : sessionName : _) -> Right (Begin sessionName)
            ("load" : fileName : _) -> Right (Load fileName)
            ("batch" : fileName : _) -> Right (BatchFile fileName)
            ["end"] -> Right End
            ["help"] -> Right Help
            _ -> Left "Unknown command"



saveRecipes :: FilePath -> [Recipe] -> IO ()
saveRecipes filePath recipes = do
    writeFile filePath (unlines $ map serializeRecipe recipes)

loadRecipes :: FilePath -> IO [Recipe]
loadRecipes filePath = do
    contents <- readFile filePath
    return $ mapMaybe deserializeRecipe (lines contents)

serializeRecipe :: Recipe -> String
serializeRecipe (Recipe name ingredients) =
    name ++ ":" ++ unwords (map serializeIngredient ingredients)

deserializeRecipe :: String -> Maybe Recipe
deserializeRecipe input =
    case break (== ':') input of
        (name, ':' : rest) -> Just (Recipe name (mapMaybe deserializeIngredient (words rest)))
        _ -> Nothing

serializeIngredient :: Ingredient -> String
serializeIngredient (Ingredient name qty cal) =
    name ++ "," ++ show qty ++ "," ++ show cal

deserializeIngredient :: String -> Maybe Ingredient
deserializeIngredient input =
    case words $ map (\c -> if c == ',' then ' ' else c) input of
        [name, qty, cal] -> Just (Ingredient name (read qty) (read cal))
        _ -> Nothing

batchProcessCommands :: (Command -> [Recipe] -> IO (Bool, [Recipe]))
                     -> String
                     -> [Recipe]
                     -> IO (Bool, [Recipe], [String])
batchProcessCommands executeCommand input recipes = foldM processLine (True, recipes, []) (lines input)
  where
    processLine (True, currentRecipes, errors) line =
        case parseExtendedCommand line of
            Left err -> return (True, currentRecipes, errors ++ [err])
            Right (Command cmd) -> do
                (continue, newRecipes) <- executeCommand cmd currentRecipes
                if continue
                    then return (True, newRecipes, errors)
                    else return (False, currentRecipes, errors ++ ["Batch processing interrupted by exit command."])
            Right (Save fileName) -> do
                saveRecipes fileName currentRecipes
                return (True, currentRecipes, errors ++ ["Recipes saved to file: " ++ fileName])
            Right (Load fileName) -> do
                fileExists <- doesFileExist fileName
                if fileExists
                    then do
                        loadedRecipes <- loadRecipes fileName
                        return (True, loadedRecipes, errors ++ ["Recipes loaded from file: " ++ fileName])
                    else return (True, currentRecipes, errors ++ ["File not found: " ++ fileName])
            Right Help -> return (True, currentRecipes, errors ++ ["Help command is not supported in batch mode."])
            Right (BatchFile _) -> return (True, currentRecipes, errors ++ ["Nested batch files are not supported."])
            _ -> return (True, currentRecipes, errors ++ ["Unknown batch command"])
    processLine result _ = return result


executeBatchCommand :: Command -> [Recipe] -> Either String [Recipe]
executeBatchCommand (Add (AddRecipe name ingredients)) recipes =
    if any ((== name) . recipeName) recipes
    then Left $ "Recipe " ++ name ++ " already exists."
    else Right (Recipe name ingredients : recipes)
executeBatchCommand _ recipes = Right recipes

processGroupedCommands :: (Command -> [Recipe] -> IO (Bool, [Recipe]))
                       -> [ExtendedCommand]
                       -> [Recipe]
                       -> IO ([Recipe], [String])
processGroupedCommands executeCommand commands recipes =
    foldM (executeGroupedCommand executeCommand) (recipes, []) commands


executeGroupedCommand :: (Command -> [Recipe] -> IO (Bool, [Recipe]))
                      -> ([Recipe], [String])
                      -> ExtendedCommand
                      -> IO ([Recipe], [String])
executeGroupedCommand executeCommand (currentRecipes, errors) (Command cmd) = do
    (continue, newRecipes) <- executeCommand cmd currentRecipes
    if continue
        then return (newRecipes, errors)
        else return (currentRecipes, errors ++ ["Batch processing interrupted."])
executeGroupedCommand _ result _ = return result

