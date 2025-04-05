module Main (main) where

import Multidir.Commands.Discover (parseArgsDiscover)
import Multidir.Commands.List (parseArgsList)
import Multidir.Commands.Register (parseArgsRegister)
import Multidir.Commands.Run (parseArgsRun)
import Multidir.Commands.Where (parseArgsWhere)
import Multidir.ReadConfig
import Multidir.RunTask (taskParser)
import Options.Applicative
import System.Directory (getCurrentDirectory)


data Config = Config {
  configProjs :: Maybe String,
  configTasks :: Maybe String,
  configOtherArgs :: [String]
}

projsOption :: Parser (Maybe String)
projsOption = option (Just <$> str) (
  long "projs" <>
  short 'P' <>
  metavar "TOML" <>
  value Nothing <>
  help "Location of projects .toml file")

tasksOption :: Parser (Maybe String)
tasksOption = option (Just <$> str) (
  long "tasks" <>
  short 'T' <>
  metavar "TOML" <>
  value Nothing <>
  help "Location of tasks .toml file")

-- | The first phase of the argument parsing: are there instructions
-- on where to find the config files?
-- We search for the config files in the following order:
-- 1. from the command line
-- 2. from the environment variables
-- 3. from .config/multidir
parseConfig :: ParserInfo Config
parseConfig = let

  configParser = liftA3 Config projsOption tasksOption (many (strArgument mempty))

  information =
    progDesc "This message should not be seen" <>
    forwardOptions

  in info configParser information


parseArgs :: FilePath -> Tasks -> FilePath -> Projs -> FilePath -> ParserInfo (IO ())
parseArgs _tasksFile tasks projsFile projs wd = let

  -- --help option (needed at least if running via Stack, harmless otherwise I think)
  helpOption :: Parser (a -> a)
  helpOption =
    abortOption (ShowHelpText Nothing) (
      long "help" <>
      short 'h' <>
      help "Show this help text")

  information =
    fullDesc <>
    progDesc "Run commands in multiple directories"

  commands =
    command "discover" (parseArgsDiscover wd projsFile) <>
    command "list" (parseArgsList projs wd) <>
    command "register" (parseArgsRegister wd projsFile) <>
    command "run" (parseArgsRun projs wd) <>
    command "where" parseArgsWhere

  parser =
    projsOption *> -- this is only for the help text
    tasksOption *> -- this is only for the help text
    helpOption <*>
    (hsubparser commands <|> taskParser tasks projs wd)

  in info parser information


main :: IO ()
main = do
  wd <- getCurrentDirectory
  conf <- execParser parseConfig
  tasksFile <- tasksFilename $ configTasks conf
  tasks <- readTasks tasksFile
  projsFile <- projsFilename $ configProjs conf
  projs <- readProjs projsFile
  let remainingArgs = configOtherArgs conf
  let parser = parseArgs tasksFile tasks projsFile projs wd
  go <- handleParseResult $ execParserPure defaultPrefs parser remainingArgs
  go
