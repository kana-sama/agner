import Named

import Data.Bits (shiftR, Bits ((.&.)))
import Data.Aeson (encodeFile)

import System.Process.Typed (runProcess, shell, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), stripExtension)
import System.Environment (getArgs)
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

run :: forall e. Exception e => IO Value -> IO ()
run value = do
  result <- try @e (evaluate =<< value)
  case result of
    Left e -> putStrLn (displayException e)
    Right v -> putStrLn (Value.encode v)

parseArgs :: IO Command
parseArgs = do
  getArgs >>= \case
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

compile ::  
  "target" :! X64.Target ->
  "source" :! FilePath ->
  "output" :! FilePath ->
  IO ()
compile (Arg target) (Arg source) (Arg output) = do
  source <- parse        source
  source <- lint         source
  source <- optimize     source
  sm     <- compileToSM  source
  x64    <- compileToX64 sm
  compileToBinary output x64
  where
    parse :: FilePath -> IO Syntax.Module
    parse source =
      try @Parser.Ex (evaluate =<< Parser.parse Parser.module_ <$> readFile source) >>= \case
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

        let runtimePath = "." </> "runtime" </> "runtime.c"
        let sourcePath = path </> "temp" <.> "s"
        let outputPath = output

        writeFile sourcePath gas
        gcc ! param #target target
            ! param #source sourcePath
            ! param #runtime runtimePath
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

example :: X64.Target -> IO ()
example target = do
  -- parse
  !source <- try @Parser.Ex (evaluate =<< Parser.parse Parser.module_ <$> readFile "example.agn") >>= \case
    Right source -> pure source
    Left ex -> do putStrLn (displayException ex); exitFailure
  Prettier.io Prettier.module_ source
  putStrLn ""

  -- lint
  case Linter.check source of
    Nothing -> pure ()
    Just error -> do
      putStrLn ("** linter error: " ++ Linter.prettyError error)
      exitFailure

  -- optimize
  source <- pure (Optimizer.optimize source)

  -- eval
  putStrLn "denote:"
  run @Denote.Ex (Denote.module_ source)

  -- eval vm
  putStrLn "stack machine:"
  let sm = SM.compileModule source
  -- let !debug = SM.debug 1000 sm
  -- encodeFile "adbg/src/debug.json" debug
  -- putStrLn "debug done"
  run @SM.Ex (SM.run sm)

  -- compile and run
  withSystemTempDirectory "test" \path -> do
    let gas = (X64.prettyProg . X64.compile target) sm

    writeFile "output.s" gas
    putStrLn "output.s dumped"

    let runtimePath = "." </> "runtime" </> "runtime.c"
    let sourcePath = path </> "temp" <.> "s"
    let outputPath = path </> "temp"

    writeFile sourcePath gas
    gcc ! param #target target
        ! param #source sourcePath
        ! param #runtime runtimePath
        ! param #output outputPath

    putStrLn "x64:"
    runProcess (shell outputPath) >>= \case
      ExitFailure (-11) -> putStrLn "сегфолт"
      ExitFailure i -> putStrLn ("ExitCode = " ++ show i)
      ExitSuccess -> pure ()

gcc ::
  "target" :! X64.Target ->
  "source" :! FilePath ->
  "runtime" :! FilePath ->
  "output" :! FilePath ->
  IO ExitCode
gcc (Arg target) (Arg source) (Arg runtime) (Arg output) = case target of
  X64.MacOS -> runProcess (proc "gcc" [runtime, source, "-o", output])
  X64.Linux -> runProcess (proc "gcc" ["-z", "noexecstack", runtime, source, "-o", output])
