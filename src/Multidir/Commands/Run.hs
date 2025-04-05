-- | Run an arbitrary command
--
-- Also used by RunTask
module Multidir.Commands.Run (
  runString,
  runStrings,
  parseArgsRun,
  ) where

import qualified Data.Map.Strict as M
import Multidir.Iterate (onAll)
import Multidir.ReadConfig
import Multidir.Selection (applySelection, optSelection)
import Options.Applicative
import System.Exit (ExitCode(..))
import System.Process


runString :: String -> Proj -> IO Bool
runString s p = do
  let c = (shell s) {
    cwd = Just $ projDir p,
    env = Just . M.assocs $ projEnv p
  }
  (_, _, _, h) <- createProcess c
  x <- waitForProcess h
  putStrLn ""
  pure $ case x of
    ExitSuccess -> True
    ExitFailure _ -> False

runStrings :: [String] -> Proj -> IO Bool
runStrings [] _ = pure True
runStrings (s:t) p = do
  f <- runString s p
  if f
    then runStrings t p
    else pure False

parseArgsRun :: Projs -> FilePath -> ParserInfo (IO ())
parseArgsRun projs wd = let
  information = progDesc "Run a shell script in each directory"
  argCommand = strArgument (
    metavar "SHELLSCRIPT")
  run s f = onAll (runString s) (applySelection f wd projs)
  parser = liftA2 run argCommand optSelection
  in info parser information
