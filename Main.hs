import Paths_agner (getDataDir)
import Shower (shower)

import Data.IORef (atomicModifyIORef', newIORef)

import System.Process.Typed (runProcess, proc)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), takeFileName, takeBaseName)
import System.FilePath.Glob (globDir1)
import System.Environment (getArgs)
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)

import Control.Exception (evaluate)
import Control.Lens (ifor)
import Control.Monad (when)

import Language.Agner.X64 qualified as X64
import Language.Agner.Parser qualified as Parser
import Language.Agner.Process qualified as Process

data Command
  = Compile{source :: [FilePath], output :: FilePath}

parseArgs :: IO Command
parseArgs = do
  getArgs >>= \case
    "-o" : output : source -> pure Compile{source, output}
    _ -> error "wrong arguments"

getRuntimeSourceFiles :: IO [FilePath]
getRuntimeSourceFiles = do
  dataDir <- getDataDir
  globDir1 "**/*.c" (dataDir </> "runtime")

getStdLibSourceFiles :: IO [FilePath]
getStdLibSourceFiles = do
  dataDir <- getDataDir
  globDir1 "**/*.erl" (dataDir </> "stdlib")

target :: X64.Target
target = case os of
  "linux" -> X64.Linux
  "darwin" -> X64.MacOS
  _ -> error "Иди отсюда, пёс"

main :: IO ()
main = do
  Compile{source, output} <- parseArgs
  stdLibSourceFiles <- getStdLibSourceFiles
  let sourceFiles = source ++ stdLibSourceFiles

  stepIndex <- newIORef 1
  let totalSteps = length sourceFiles + 2 -- entry point + executable
  let printStep = do
        step <- atomicModifyIORef' stepIndex \i -> (i + 1, i)
        let padding = length (show totalSteps) - length (show step)
        putStr ("[" ++ replicate padding ' ' ++ show step ++ " of " ++ show totalSteps ++ "] ")

  withSystemTempDirectory "dist" \temp -> do
    (ctx, asmFiles) <- mconcat <$> ifor sourceFiles \index sourceFile -> do
      printStep; putStrLn ("Compiling " ++ takeFileName sourceFile)

      module_ <- readFile sourceFile
      module_ <- evaluate (Parser.parse sourceFile Parser.module_ module_)
      module_ <- evaluate (Process.process module_)
      let processedModule = module_

      (ctx, module_) <- pure (X64.compileModule target module_)
      module_ <- evaluate module_

      let file_asm = temp </> show index <.> "S"
      writeFile file_asm module_

      when (sourceFile `notElem` stdLibSourceFiles) do
        writeFile ("agner-output" </> takeBaseName sourceFile <.> ".X") (shower processedModule)
        writeFile ("agner-output" </> takeBaseName sourceFile <.> ".S") module_

      pure (ctx, [file_asm])

    printStep; putStrLn "Compiling entry point"
    entry <- evaluate (X64.compileEntryPoint target ctx)
    let entryFile = temp </> "entry" <.> "S"
    writeFile entryFile entry

    printStep; putStrLn "Building executable"
    runtimeSourceFiles <- getRuntimeSourceFiles
    let files_to_compile = [entryFile] ++ runtimeSourceFiles ++ asmFiles
    ExitSuccess <- runProcess do proc "clang" ("-O3" : "-o" : output : files_to_compile)
    pure ()
