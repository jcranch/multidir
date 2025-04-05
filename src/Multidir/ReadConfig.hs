{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Multidir.ReadConfig (

  Specification(..),

  Task(..),
  Tasks(..),
  tasksFilename,
  readTasks,
  emptyTasks,
  
  Proj(..),
  Projs(..),
  projsFilename,
  readProjs,
  isProj,
  emptyProjs,

  Recipe(..),
  Recipes(..),
  recipesFilename,
  readRecipes,
  emptyRecipes,
  ) where

import Control.Exception (IOException, catch)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import TOML.Decode
import TOML.Decode.Extra
import TOML.Error
import TOML.Value


data Specification = Specification {
  specCommand :: String,
  specEnv :: Map String String
}

instance DecodeTOML Specification where
  tomlDecoder = decodeTableWith $ \t -> do
    (t', e) <- removeFieldOpt "env" t
    (_, r) <- removeField "run" t'
    pure $ Specification {
      specCommand = r,
      specEnv = fromMaybe M.empty e
    }


data Task = Task {
  taskDescription :: Maybe Text,
  taskSpecByTag :: Map Text Specification
}

instance DecodeTOML Task where
  tomlDecoder = decodeTableWith $ \t -> do
    (t', description) <- removeFieldOptWith tomlDecoder "description" t
    byTag <- processTableValuesWith tomlDecoder t'
    pure $ Task {
      taskDescription = description,
      taskSpecByTag = byTag
    }

newtype Tasks = Tasks {
  byTask :: Map Text Task
}

emptyTasks :: Tasks
emptyTasks = Tasks mempty


instance DecodeTOML Tasks where
  tomlDecoder = Tasks <$> getTableOf tomlDecoder


data Proj = Proj {
  projDir :: FilePath,
  projTags :: [Text],
  projEnv :: Map String String
} deriving (Show)


newtype Projs = Projs {
  getProjs :: [Proj]
}

emptyProjs :: Projs
emptyProjs = Projs mempty

isProj :: FilePath -> Projs -> Bool
isProj p = any ((== p) . projDir) . getProjs


decodeProj :: Text -> Table -> DecodeM Proj
decodeProj n t = do

  -- Pull out the "tags" field
  (t', l) <- removeFieldOptWith tomlDecoder "tags" t

  -- The remaining fields are config
  c <- processTableValuesWith tomlDecoder t'

  pure $ Proj {
    projDir = T.unpack n,
    projTags = fromMaybe [] l,
    projEnv = M.mapKeys T.unpack c
  }


instance DecodeTOML Projs where
  tomlDecoder = let
    f (n, t) = addContextItem (Key n) $ decodeTableWithM (decodeProj n) t
    in decodeTableWith (fmap Projs . traverse f . M.assocs)


data Recipe = Recipe {
  recipeTags :: [Text],
  recipeFiles :: [String],
  recipeGlobs :: [String],
  recipeDiscover :: Bool
} deriving (Show)

instance DecodeTOML Recipe where
  tomlDecoder = Recipe <$>
    getField "tags" <*>
    (fromMaybe [] <$> getFieldOpt "files") <*>
    (fromMaybe [] <$> getFieldOpt "globs") <*>
    (fromMaybe True <$> getFieldOpt "discover")

newtype Recipes = Recipes {
  getRecipes :: [Recipe]
} deriving (Show)

instance DecodeTOML Recipes where
  tomlDecoder = Recipes <$> getField "recipes"

emptyRecipes :: Recipes
emptyRecipes = Recipes []



-- | Decode filename
-- Returns `Nothing` if file not found
decodeFileIfFound :: DecodeTOML a => FilePath -> IO (Maybe (Either TOMLError a))
decodeFileIfFound p = let
  toml = fmap Just (T.readFile p) `catch` (\e -> const (pure Nothing) (e :: IOException))
  in fmap decode <$> toml


-- | Decode filename; exits in case of error, uses default in case of file not found
decodeFileWithDefault :: DecodeTOML a => a -> FilePath -> IO a
decodeFileWithDefault d p = do
  a <- decodeFileIfFound p
  case a of
    Just (Left e) -> do
      putStrLn ("Error in " <> p <> ": " <> show e)
      exitFailure
    Nothing -> pure d
    Just (Right x) -> pure x


infixr 2 <||>
-- | An idiom for "failing that"
(<||>) :: Monad m => m (Maybe a) -> m a -> m a
u <||> v = maybe v pure =<< u
  

getConfigFilename :: FilePath -> String -> Maybe FilePath -> IO FilePath
getConfigFilename fileName envVar fromArgs = let
  fromEnv = lookupEnv envVar
  fromXDG = fmap ((</> fileName) . (</> "muld")) <$> lookupEnv "XDG_CONFIG_HOME"
  fromHome = fmap ((</> fileName) . (</> ".config/muld")) <$> lookupEnv "HOME"
  stuck = pure $ error "Don't know where to find config filename"
  in pure fromArgs <||> fromEnv <||> fromXDG <||> fromHome <||> stuck

tasksFilename :: Maybe FilePath -> IO FilePath
tasksFilename = getConfigFilename "tasks.toml" "MULD_TASK_FILE"

projsFilename :: Maybe FilePath -> IO FilePath
projsFilename = getConfigFilename "projs.toml" "MULD_PROJ_FILE"

recipesFilename :: Maybe FilePath -> IO FilePath
recipesFilename = getConfigFilename "recipes.toml" "MULD_RECIPE_FILE"


readTasks :: FilePath -> IO Tasks
readTasks = decodeFileWithDefault emptyTasks

readProjs :: FilePath -> IO Projs
readProjs = decodeFileWithDefault emptyProjs

readRecipes :: FilePath -> IO Recipes
readRecipes = decodeFileWithDefault emptyRecipes
