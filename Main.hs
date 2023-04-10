{-# LANGUAGE OverloadedLists #-}

import Named

import Paths_agner (getDataFileName)

import System.Process.Typed (runProcess, shell, proc)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), stripExtension)
import System.Environment (getArgs, setEnv)
import System.Info (os)

import Control.Exception (evaluate, try, displayException)

import Language.Agner.SM qualified as SM
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
      [ "list"
      , "options"
      , "value"
      , "throw"
      , "heap"
      , "mailbox"
      , "process"
      , "scheduler"
      , "runtime"
      , "bifs"
      , "operators"
      , "shared_atoms"
      ]

gcc ::
  "target" :! X64.Target ->
  "source" :! [FilePath] ->
  "output" :! FilePath ->
  IO ExitCode
gcc (Arg target) (Arg source) (Arg output) = case target of
  X64.MacOS -> runProcess (proc "gcc" (["-o", output] ++ source))
  X64.Linux -> runProcess (proc "gcc" (["-o", output] ++ source ++ ["-z", "noexecstack"]))

compile ::  
  "target" :! X64.Target ->
  "source" :! FilePath ->
  "output" :! FilePath ->
  "asm"    :! FilePath ->
  IO ()
compile (Arg target) (Arg sourcePath) (Arg outputPath) (Arg asmPath) = do
  sourceCode <- readFile     sourcePath
  source     <- parse        sourceCode
  source     <- optimize     source
  sm         <- compileToSM  source
  x64        <- compileToX64 sm
  _          <- compileToBinary x64
  pure ()
  where
    parse :: String -> IO Syntax.Module
    parse source =
      try (evaluate (Parser.parse Parser.module_ source)) >>= \case
        Right source -> pure source
        Left (ex :: Parser.Ex) -> do putStrLn (displayException ex); exitFailure

    optimize :: Syntax.Module -> IO Syntax.Module
    optimize module_ = pure (Optimizer.optimize module_)

    compileToSM :: Syntax.Module -> IO SM.Prog
    compileToSM module_ = pure (SM.compileModule module_)

    compileToX64 :: SM.Prog -> IO X64.Prog
    compileToX64 prog = pure (X64.compile target prog)

    compileToBinary :: X64.Prog -> IO ()
    compileToBinary prog = do
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