module Multidir.Commands.List (
  parseArgsList,
  ) where

import Data.Foldable (traverse_)
import Multidir.ReadConfig
import Multidir.Selection (Selection, applySelection, optSelection)
import Options.Applicative


runList :: Projs -> FilePath -> Selection -> IO ()
runList projs wd f = traverse_ (putStrLn . projDir) . getProjs $ applySelection f wd projs

parseArgsList :: Projs -> FilePath -> ParserInfo (IO ())
parseArgsList projs wd = let
  information = progDesc "List directories"
  parser = runList projs wd <$> optSelection
  in info parser information
