-- | Add a repository
module Multidir.Commands.Register (
  investigate,
  writeProj,
  parseArgsRegister,
  ) where

import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import Multidir.ReadConfig
import Options.Applicative
import System.Directory (doesPathExist, makeAbsolute)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.FilePattern.Directory


andM :: Monad m => (a -> m Bool) -> [a] -> m Bool
andM _ [] = pure True
andM f (x:xs) = do
  a <- f x
  if a then andM f xs else pure False


-- | A common base for the registering/discovering functionality
-- Returns tags, and whether it was worthy of being discovered.
investigate :: FilePath -> Recipes -> IO ([Text], Bool)
investigate fp = let

  matchGlob :: FilePattern -> IO Bool
  matchGlob g = not . null <$> getDirectoryFiles fp [g]

  matchFile :: String -> IO Bool
  matchFile f = doesPathExist (fp </> f)

  new [] x (tagList, tagSet, discovered) = (tagList, tagSet, discovered || x)
  new (t:ts) x (tagList, tagSet, discovered)
    | S.member t tagSet = new ts x (tagList, tagSet, discovered)
    | otherwise = new ts x (t:tagList, S.insert t tagSet, discovered)

  try d m = do
    b <- liftA2 (&&) (andM matchFile (recipeFiles m)) (andM matchGlob (recipeGlobs m))
    pure $ if b
      then new (recipeTags m) (recipeDiscover m) d
      else d

  finalise (tagList, _, discovered) = (reverse tagList, discovered)

  in fmap finalise . foldlM try ([], S.empty, False) . getRecipes


writeProj :: FilePath -> [Text] -> FilePath -> IO ()
writeProj dir tags target = appendFile target (
  "[" <> show dir <> "]\n" <>
  "tags = " <> show tags <> "\n\n")


register :: Bool -> Maybe FilePath -> FilePath -> FilePath -> IO ()
register force recipesDir rawDir target = do
  dir <- makeAbsolute rawDir
  recipes <- readRecipes =<< recipesFilename recipesDir
  (tags, discover) <- investigate dir recipes
  if force || discover
    then do
      putStrLn (show tags)
      writeProj dir tags target
    else do
      putStrLn "Nothing to register"
      exitFailure


parseArgsRegister :: FilePath -> FilePath -> ParserInfo (IO ())
parseArgsRegister wd projsFile = let

  information = progDesc "Add a project directory"

  forceSwitch = switch (
    long "force" <>
    short 'f' <>
    help "Add even if we wouldn't normally add such a directory")

  recipesOpt = option (Just <$> str) (
    long "recipes" <>
    short 'R' <>
    metavar "TOML" <>
    value Nothing <>
    help "Location of recipes .toml file")

  dirArgument = strArgument (
    metavar "DIR" <>
    help "Directory to add")

  dirArgumentOpt = fromMaybe wd <$> optional dirArgument

  targetOption = strOption (
    long "output" <>
    short 'o' <>
    help "File to write to (if not config file)" <>
    value projsFile)

  parser = register <$> forceSwitch <*> recipesOpt <*> dirArgumentOpt <*> targetOption

  in info parser information
