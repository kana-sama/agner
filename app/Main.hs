import Data.Bits (shiftR)

import System.Process.Typed (runProcess, shell, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))

import Control.Exception (try, evaluate)

import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Denote qualified as Denote
import Language.Agner.SM qualified as SM
import Language.Agner.X64 qualified as X64
import Language.Agner.Parser qualified as Parser
import Language.Agner.Pretty qualified as Pretty

main = do
  !source <- Parser.parse Parser.module_ <$> readFile "example.agn"
  putStrLn "source:"
  print (Pretty.module_ source)

  putStrLn "denote:"
  result <- try @Denote.Ex do evaluate (Value.encode (Denote.module_ source))
  print result

  putStrLn "stack machine:"
  let !sm = SM.compileModule source
  result <- try @SM.Ex do evaluate (Value.encode (SM.run sm))
  print result

  withSystemTempDirectory "test" \path -> do
    let gas = (X64.prettyProg . X64.compile) sm

    let sourcePath = path </> "temp" <.> "s"
    let outputPath = path </> "temp"

    writeFile sourcePath gas
    writeFile "output.s" gas
    runProcess (proc "gcc-12" [sourcePath, "-o", outputPath])

    code <- runProcess (shell outputPath)
    putStrLn "x64:"
    putStrLn (Value.encode (exitCodeToValue code))

    -- putStrLn ("\n" ++ gas)

exitCodeToValue :: ExitCode -> Value
exitCodeToValue = \case
  ExitSuccess -> Value.Integer 0
  ExitFailure i -> Value.Integer ((fromIntegral i) `shiftR` 3)