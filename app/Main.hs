{-# LANGUAGE OverloadedLists #-}

import Named

import Paths_agner (getDataFileName)

import Data.Bits (shiftR, Bits ((.&.)))
import Data.Aeson (encodeFile)
import Data.IORef (newIORef, readIORef)
import Data.ByteString.Lazy qualified as ByteString
import Data.Csv qualified as CSV

import System.Process.Typed (runProcess, shell, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), stripExtension)
import System.Environment (getArgs, setEnv)
import System.Info (os)

import Control.Exception (evaluate, try, Exception (displayException), SomeException)

import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Denote qualified as Denote
import Language.Agner.SM qualified as SM
import Language.Agner.X64 qualified as X64
import Language.Agner.Parser qualified as Parser
import Language.Agner.Prettier qualified as Prettier
import Language.Agner.Linter qualified as Linter
import Language.Agner.Optimizer qualified as Optimizer

data Command
  = Compile{target :: Maybe X64.Target, source :: FilePath, output :: Maybe FilePath}
  | Example{target :: Maybe X64.Target}
  | Pretty{source :: FilePath}

run :: forall e. Exception e => IO () -> IO ()
run value = do
  result <- try @e (evaluate =<< value)
  case result of
    Left e -> putStrLn (displayException e)
    Right v -> pure ()

parseArgs :: IO Command
parseArgs = do
  getArgs >>= \case
    ["pretty", source] -> pure Pretty{source}
    [] -> pure Example{target = Nothing}
    ["--target", "linux"] -> pure Example{target = Just X64.Linux}
    ["--target", "macos"] -> pure Example{target = Just X64.MacOS}
    [source] -> pure Compile{target = Nothing, source, output = Nothing}
    [source, "-o", output] -> pure Compile{target = Nothing, source, output = Just output}
    [source, "-o", output] -> pure Compile{target = Nothing, source, output = Just output}
    _ -> error "неправильные аргументы"

getTarget :: Maybe X64.Target -> X64.Target
getTarget (Just target) = target
getTarget Nothing =
  case os of
    "linux" -> X64.Linux
    "darwin" -> X64.MacOS
    _ -> error "Иди отсюда, пёс"

getOutput :: FilePath -> Maybe FilePath -> FilePath
getOutput source (Just path) = path
getOutput source Nothing =
  case stripExtension "agn" source of
    Nothing -> error "unknown extension"
    Just path -> path

runtime :: IO [FilePath]
runtime = traverse (getDataFileName . rt) sources
  where
    rt f = "." </> "ARTS" </> f <.> "c"
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
      ]

compile ::  
  "target" :! X64.Target ->
  "source" :! FilePath ->
  "output" :! FilePath ->
  IO ()
compile (Arg target) (Arg sourcePath) (Arg output) = do
  sourceCode <- readFile     sourcePath
  source     <- parse        sourceCode
  source     <- lint         source
  source     <- optimize     source
  sm         <- compileToSM  source
  x64        <- compileToX64 sm
  compileToBinary output x64
  where
    parse :: String -> IO Syntax.Module
    parse source =
      try @Parser.Ex (evaluate (Parser.parse Parser.module_ source)) >>= \case
        Right source -> pure source
        Left ex -> do putStrLn (displayException ex); exitFailure

    lint :: Syntax.Module -> IO Syntax.Module
    lint module_ =
      case Linter.check module_ of
        Nothing -> pure module_
        Just error -> do
          putStrLn ("** linter error: " ++ Linter.prettyError error)
          exitFailure

    optimize :: Syntax.Module -> IO Syntax.Module
    optimize module_ = pure (Optimizer.optimize module_)

    compileToSM :: Syntax.Module -> IO SM.Prog
    compileToSM module_ = pure (SM.compileModule module_)

    compileToX64 :: SM.Prog -> IO X64.Prog
    compileToX64 prog = pure (X64.compile target prog)

    compileToBinary :: FilePath -> X64.Prog -> IO ()
    compileToBinary output prog = do
      withSystemTempDirectory "test" \path -> do
        let gas = X64.prettyProg prog

        let sourcePath = path </> "temp" <.> "s"
        let outputPath = output

        writeFile sourcePath gas
        runtime <- runtime
        gcc ! param #target target
            ! param #source sourcePath
            ! param #runtime runtime
            ! param #output outputPath

      pure ()


main :: IO ()
main = parseArgs >>= \case
  Compile{target, source, output} ->
    compile ! param #target (getTarget target)
            ! param #source source
            ! param #output (getOutput source output)
  Example{target} ->
    example (getTarget target)
  Pretty{source} -> pretty source

pretty :: FilePath -> IO ()
pretty source = do
  !source <- try @Parser.Ex (evaluate =<< Parser.parse Parser.module_ <$> readFile source) >>= \case
    Right source -> pure source
    Left ex -> do putStrLn (displayException ex); exitFailure
  Prettier.io Prettier.module_ source

instance CSV.ToNamedRecord Denote.YLog where
  toNamedRecord ylog = CSV.namedRecord
    [ "id" CSV..= ylog.id
    , "pid" CSV..= ylog.pid
    , "function" CSV..= ylog.name
    , "fuel" CSV..= ylog.fuel
    ]

example :: X64.Target -> IO ()
example target = do
  -- parse
  !source <- try @Parser.Ex (evaluate =<< Parser.parse Parser.module_ <$> readFile "example.agn") >>= \case
    Right source -> pure source
    Left ex -> do putStrLn (displayException ex); exitFailure
  Prettier.io Prettier.module_ source
  putStrLn ""

  -- lint
  -- case Linter.check source of
  --   Nothing -> pure ()
  --   Just error -> do
  --     putStrLn ("** linter error: " ++ Linter.prettyError error)
  --     exitFailure

  -- optimize
  source <- pure (Optimizer.optimize source)

  -- eval
  putStrLn "denote:"
  -- run @Denote.Ex do
  --   ylogs <- Denote.denoteWithYLogs (Denote.module_ source)
  --   let csv = CSV.encodeByName ["id", "pid", "function", "fuel"] ylogs
  --   ByteString.writeFile "log_denote.csv" csv
  --   pure ()

  -- compile to SM
  let sm = SM.compileModule source

  -- compile and run
  withSystemTempDirectory "test" \path -> do
    let gas = (X64.prettyProg . X64.compile target) sm

    writeFile "output.s" gas
    putStrLn "output.s dumped"

    let sourcePath = path </> "temp" <.> "s"
    let outputPath = path </> "temp"

    writeFile sourcePath gas
    runtime <- runtime
    gcc ! param #target target
        ! param #source sourcePath
        ! param #runtime runtime
        ! param #output outputPath

    putStrLn "x64:"
    setEnv "ARTS_FUEL" "10"
    setEnv "ARTS_YLOG" "log_x64.csv"
    runProcess (shell outputPath) >>= \case
      ExitFailure (-11) -> putStrLn "сегфолт"
      ExitFailure i -> putStrLn ("ExitCode = " ++ show i)
      ExitSuccess -> pure ()

gcc ::
  "target" :! X64.Target ->
  "source" :! FilePath ->
  "runtime" :! [FilePath] ->
  "output" :! FilePath ->
  IO ExitCode
gcc (Arg target) (Arg source) (Arg runtime) (Arg output) = case target of
  X64.MacOS -> runProcess (proc "gcc" (runtime ++ [source, "-o", output]))
  X64.Linux -> runProcess (proc "gcc" (runtime ++ ["-z", "noexecstack", source, "-o", output]))
