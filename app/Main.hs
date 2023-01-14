import System.Process.Typed (runProcess, shell, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))

import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Interpreter qualified as Interpreter
import Language.Agner.SM qualified as SM
import Language.Agner.X64 qualified as X64
import Language.Agner.Parser qualified as Parser

main = do
  source <- Parser.parse <$> readFile "example.agn"

  putStrLn "eval:"
  putStrLn (Value.encode (Interpreter.eval source))

  let sm = SM.compile source
  putStrLn "stack machine:"
  putStrLn (Value.encode (SM.run sm))


  withSystemTempDirectory "test" \path -> do
    let gas = (X64.prettyProg . X64.compile) sm

    let sourcePath = path </> "temp" <.> "s"
    let outputPath = path </> "temp"

    writeFile sourcePath gas
    runProcess (proc "gcc-12" [sourcePath, "-o", outputPath])

    code <- runProcess (shell outputPath)
    putStrLn "x64:"
    putStrLn (Value.encode (exitCodeToValue code))

exitCodeToValue :: ExitCode -> Value
exitCodeToValue = \case
  ExitSuccess -> Value.Integer 0
  ExitFailure i -> Value.Integer (fromIntegral i)