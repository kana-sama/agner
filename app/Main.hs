import Data.Bits (shiftR)

import System.Process.Typed (runProcess, shell, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))

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

main = do
  !source <- Parser.parse Parser.module_ <$> readFile "example.agn"
  putStrLn "source:"
  print (Pretty.module_ source)

  putStrLn "denote:"
  run @Denote.Ex (Denote.module_ source)

  putStrLn "stack machine:"
  let !sm = SM.compileModule source
  run @SM.Ex (SM.run sm)

  withSystemTempDirectory "test" \path -> do
    let gas = (X64.prettyProg . X64.compile) sm

    let sourcePath = path </> "temp" <.> "s"
    let outputPath = path </> "temp"

    writeFile sourcePath gas
    runProcess (proc "gcc-12" [sourcePath, "-o", outputPath])

    putStrLn "x64:"
    code <- runProcess (shell outputPath)
    putStrLn (Value.encode (exitCodeToValue code))

    writeFile "output.s" gas

exitCodeToValue :: ExitCode -> Value
exitCodeToValue = \case
  ExitSuccess -> Value.Integer 0
  ExitFailure i -> Value.Integer (fromIntegral i `shiftR` 3)