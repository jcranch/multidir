-- | Run a task
module Multidir.RunTask (
  runTask,
  taskParser,
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Multidir.Commands.Run (runString)
import Multidir.Iterate (onAll)
import Multidir.ReadConfig
import Multidir.Selection (applySelection, optSelection)
import Options.Applicative


getSpec :: Task -> Proj -> Maybe Specification
getSpec t p = let
  inner [] = Nothing
  inner (x:xs) = case M.lookup x (taskSpecByTag t) of
    Just s -> Just s
    Nothing -> inner xs
  in inner (projTags p)


-- | Add extra environment from the Specification into the Proj
withExtraEnv :: Specification -> Proj -> Proj
withExtraEnv s p = p {
  projEnv = M.union (projEnv p) (specEnv s)
  }


runTask :: Task -> Proj -> IO Bool
runTask t p = case getSpec t p of
  Nothing -> False <$ putStrLn "No specification"
  Just s -> runString (specCommand s) (withExtraEnv s p)


taskParser :: Tasks -> Projs -> FilePath -> Parser (IO ())
taskParser tasks projs wd = let

  taskCommand c t = let

    run f = onAll (runTask t) (applySelection f wd projs)

    parser = run <$> optSelection

    information = case taskDescription t of
      Just d -> progDesc $ T.unpack d
      Nothing -> mempty

    in command (T.unpack c) $ info parser information

  in hsubparser (M.foldMapWithKey taskCommand (byTask tasks) <>
                 commandGroup "User-defined commands:" <>
                 hidden)
