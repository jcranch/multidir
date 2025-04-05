module Multidir.Commands.Where (
  parseArgsWhere,
  ) where

import Multidir.ReadConfig
import Options.Applicative


runWhere :: IO ()
runWhere = do
  putStr "tasks:   "
  putStrLn =<< tasksFilename Nothing
  putStr "projs:   "
  putStrLn =<< projsFilename Nothing
  putStr "recipes: "
  putStrLn =<< recipesFilename Nothing

parseArgsWhere :: ParserInfo (IO ())
parseArgsWhere = let
  information = progDesc "Show location of config files"
  in info (pure runWhere) information
