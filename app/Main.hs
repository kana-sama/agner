import Data.Bits (shiftR, Bits ((.&.)))

import System.Process.Typed (runProcess, shell, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.Environment (getArgs)
import System.Info (os)

import Control.Exception (evaluate, try, Exception (displayException))

import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Denote qualified as Denote
import Language.Agner.SM qualified as SM
import Language.Agner.X64 qualified as X64
import Language.Agner.Parser qualified as Parser
import Language.Agner.Pretty qualified as Pretty

run :: forall e. Exception e => Value -> IO ()
run value = do
  result <- try @e (evaluate value)
  case result of
    Left e -> putStrLn (displayException e)
    Right v -> putStrLn (Value.encode v)

parseArgs :: IO X64.Target
parseArgs = do
  args <- getArgs
  pure case args of
    ["--target", "linux"] -> X64.Linux
    ["--target", "macos"] -> X64.MacOS
    [] | "linux"  <-os-> X64.Linux
       | "darwin" <-os-> X64.MacOS
    _ -> error "Иди отсюда, пёс"

main :: IO ()
main = do
  !target <- parseArgs

  !source <- Parser.parse Parser.module_ <$> readFile "example.agn"
  putStrLn "source:"
  print (Pretty.module_ source)

  putStrLn "denote:"
  run @Denote.Ex (Denote.module_ source)

  putStrLn "stack machine:"
  let sm = SM.compileModule source
  run @SM.Ex (SM.run sm)

  withSystemTempDirectory "test" \path -> do
    let gas = (X64.prettyProg . X64.compile target) sm

    let runtimePath = "." </> "runtime" </> "runtime.c"
    let sourcePath = path </> "temp" <.> "s"
    let outputPath = path </> "temp"

    writeFile sourcePath gas
    gcc target sourcePath runtimePath outputPath

    putStrLn "x64:"
    runProcess (shell outputPath) >>= \case
      ExitFailure i -> putStrLn ("ExitCode = " ++ show i)
      ExitSuccess -> pure ()

    writeFile "output.s" gas

gcc :: X64.Target -> FilePath -> FilePath -> FilePath -> IO ExitCode
gcc target sourcePath runtimePath outputPath = case target of
  X64.MacOS -> runProcess (proc "gcc" [runtimePath, sourcePath, "-o", outputPath])
  X64.Linux -> runProcess (proc "gcc" ["-z", "noexecstack", runtimePath, sourcePath, "-o", outputPath])
