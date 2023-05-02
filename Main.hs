{-# LANGUAGE DataKinds #-}

import Named
import Shower (printer)

import Paths_agner (getDataDir)

import Data.Traversable (for)
import Data.Traversable.WithIndex (ifor)

import System.Process.Typed (runProcess, shell, proc)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>), stripExtension, isExtensionOf, takeFileName)
import System.FilePath.Glob (globDir1)
import System.Environment (getArgs, setEnv)
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)

import Control.Exception (evaluate, try, displayException)

import Language.Agner.X64 qualified as X64
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Parser qualified as Parser
import Language.Agner.Optimizer qualified as Optimizer
import Language.Agner.Desugarer qualified as Desugarer

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
  withSystemTempDirectory "dist" \temp -> do
    (ctx, asmFiles) <- mconcat <$> ifor sourceFiles \index sourceFile -> do
      putStrLn ("compiling " ++ takeFileName sourceFile)

      module_ <- readFile sourceFile
      module_ <- evaluate (Parser.parse sourceFile Parser.module_ module_)
      module_ <- evaluate (Desugarer.desugar module_)
      module_ <- evaluate (Optimizer.optimize module_)

      (ctx, module_) <- pure (X64.compileModule target module_)
      module_ <- evaluate module_

      let file_asm = temp </> show index <.> "s"
      writeFile file_asm module_
      
      pure (ctx, [file_asm])
    
    putStrLn "compiling entry point"
    entry <- evaluate (X64.compileEntryPoint target ctx)
    let entryFile = temp </> "entry" <.> "s"
    writeFile entryFile entry

    putStrLn "building executable"
    runtimeSourceFiles <- getRuntimeSourceFiles
    let files_to_compile = [entryFile] ++ runtimeSourceFiles ++ asmFiles
    ExitSuccess <- runProcess do proc "gcc" ("-o" : output : files_to_compile)
    putStrLn "compilation done"
