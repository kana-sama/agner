{-# LANGUAGE DataKinds #-}

import Named
import Shower (printer)

import Paths_agner (getDataFileName)


import System.Process.Typed (runProcess, shell, proc)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), stripExtension)
import System.Environment (getArgs, setEnv)
import System.Info (os)

import Control.Exception (evaluate, try, displayException)

import Language.Agner.X64 qualified as X64
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Parser qualified as Parser
import Language.Agner.Optimizer qualified as Optimizer

data Command
  = Example
  | Compile{source :: FilePath}

parseArgs :: IO Command
parseArgs = do
  getArgs >>= \case
    [] -> pure Example
    [source] -> pure Compile{source}
    _ -> error "неправильные аргументы"

getOutputPath :: FilePath -> FilePath
getOutputPath source =
  case stripExtension "agn" source of
    Nothing -> error "unknown extension"
    Just path -> path

runtimeSource :: IO [FilePath]
runtimeSource = traverse runtimeFile sources
  where
    runtimeFile f = getDataFileName ("." </> "runtime" </> f <.> "c")
    sources =
      [ "containers/list"
      , "options"
      , "value"
      , "throw"
      , "heap"
      , "mailbox"
      , "process"
      , "scheduler"
      , "runtime"
      , "primitives"
      , "operators"
      , "shared"
      , "matches"
      , "asserts"
      , "scopes"
      ]

gcc ::
  "target" :! X64.Target ->
  "source" :! [FilePath] ->
  "output" :! FilePath ->
  IO ()
gcc (Arg target) (Arg source) (Arg output) = case target of
  X64.MacOS -> gcc' []
  X64.Linux -> gcc' ["-z", "noexecstack"]
  where
    gcc' extraArgs = do
      code <- runProcess (proc "gcc" (["-o", output] ++ extraArgs ++ source))
      case code of
        ExitSuccess -> pure ()
        ExitFailure i -> error ("gcc failied with " ++ show i)

compile ::  
  "target" :! X64.Target ->
  "source" :! FilePath ->
  "output" :! FilePath ->
  "asm"    :! FilePath ->
  IO ()
compile (Arg target) (Arg source) (Arg outputPath) (Arg asmPath) = do
  source  <-  readFile  source
  source  <-  parse     source
  source  <-  optimize  source
  source  <-  compile   source
  _       <-  toBinary  source
  pure ()
  where
    parse :: String -> IO Syntax.Module
    parse source = evaluate (Parser.parse Parser.module_ source)

    optimize :: Syntax.Module -> IO Syntax.Module
    optimize module_ = pure (Optimizer.optimize module_)

    compile :: Syntax.Module -> IO X64.Prog
    compile prog = pure (X64.compile target prog)

    toBinary :: X64.Prog -> IO ()
    toBinary prog = do
      writeFile asmPath (X64.prettyProg prog)
      runtime <- runtimeSource
      gcc ! param #target target
          ! param #source (asmPath : runtime)
          ! param #output outputPath
      pure ()

example :: X64.Target -> IO ()
example target = do
  let source = "example.agn"
  compile ! param #target target
          ! param #source source
          ! param #output (getOutputPath source)
          ! param #asm    (getOutputPath source <.> "s")
  setEnv "ARTS_FUEL" "10"
  setEnv "ARTS_YLOG" "log_x64.csv"
  runProcess (shell ("." </> getOutputPath source)) >>= \case
    ExitFailure (-11) -> putStrLn "сегфолт"
    ExitFailure i -> putStrLn ("ExitCode = " ++ show i)
    ExitSuccess -> pure ()

main :: IO ()
main = parseArgs >>= \case
  Example -> example target
  Compile{source} ->
    compile ! param #target target
            ! param #source source
            ! param #output (getOutputPath source)
            ! param #asm    (getOutputPath source <.> "s")
  where
    target = case os of
      "linux" -> X64.Linux
      "darwin" -> X64.MacOS
      _ -> error "Иди отсюда, пёс"
