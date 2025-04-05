-- | Search for repositories
module Multidir.Commands.Discover (
  parseArgsDiscover,
  ) where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Multidir.Commands.Register (investigate, writeProj)
import Multidir.ReadConfig
import Options.Applicative
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))


discoverWith :: Recipes -> FilePath -> Bool -> Projs -> FilePath -> IO ()
discoverWith recipes target includeHidden alreadyKnown = let

  visible ('.':_) = includeHidden
  visible _ = True

  inner dir = do

    b <- doesDirectoryExist dir
    c <- if b then not <$> pathIsSymbolicLink dir else pure False
    if c && not (isProj dir alreadyKnown)
      then do
        (tags, discovered) <- investigate dir recipes
        if discovered
          then do
            putStrLn dir
            putStrLn ("  " <> show tags)
            putStrLn ""
            writeProj dir tags target
          else do
            traverse_ (inner . (dir </>)) . filter visible =<< listDirectory dir
      else do
        pure ()

  in inner


discover :: Maybe FilePath -> FilePath -> Bool -> Bool -> FilePath -> IO ()
discover recipesDir target includeHidden rediscover startDir = do
  recipes <- readRecipes =<< recipesFilename recipesDir
  alreadyKnown <- if rediscover
    then pure emptyProjs
    else readProjs target
  discoverWith recipes target includeHidden alreadyKnown startDir


parseArgsDiscover :: FilePath -> FilePath -> ParserInfo (IO ())
parseArgsDiscover wd projsFile = let

  information = progDesc "Search for project directories"

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

  includeHiddenSwitch = switch (
    long "include-all" <>
    short 'i' <>
    help "Include hidden directories")

  rediscoverSwitch = switch (
    long "rediscover" <>
    short 'r' <>
    help "Discover even if already listed")

  parser = discover <$> recipesOpt <*> targetOption <*> includeHiddenSwitch <*> rediscoverSwitch <*> dirArgumentOpt

  in info parser information
