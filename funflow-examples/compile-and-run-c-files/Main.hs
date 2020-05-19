{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Arrow
import Control.Exception (Exception (..))
import Control.Funflow
import qualified Control.Funflow.External.Docker as Docker
import Data.CAS.ContentStore (Content (..))
import qualified Data.CAS.ContentStore as CS
import Path
import Path.IO

main :: IO ()
main = do
    cwd <- getCurrentDir
    r <- withSimpleLocalRunner (cwd </> [reldir|funflow-examples/store|]) $ \run ->
      run mainFlow 3
    case r of
      Left err ->
        putStrLn $ "FAILED: " ++ displayException err
      Right out ->
        putStrLn $ "SUCCESS: " ++ out

-- | This flow takes a number, builds a C program to process that number,
-- and returns the program's output.
mainFlow :: SimpleFlow Int String
mainFlow = proc n -> do
    moduleDouble <- compileModule -<
      "int times2(int n) { return 2*n; }\n"
    moduleSquare <- compileModule -<
      "int square(int n) { return n*n; }\n"
    moduleMain <- compileModule -<
      "#include <stdio.h>\n\
      \#include <stdlib.h>\n\
      \int times2(int n);\n\
      \int square(int n);\n\
      \int main(int argc, char **argv) {\n\
      \  int n = atoi(argv[1]);\n\
      \  int r = times2(n) + square(n);\n\
      \  printf(\"%d\\n\", r);\n\
      \}"
    exec <- compileExec -< [moduleDouble, moduleSquare, moduleMain]
    out <- runExec -< (exec, [show n])
    readString_ -< out

-- | This flow takes a string which is assumed to be the source code
--   for a 'C' module. It writes this to a file, then uses an external
--   step to compile the module.
compileModule :: SimpleFlow String (Content File)
compileModule = proc csrc -> do
    cInput <- writeString -< (csrc, [relfile|out.c|])
    scriptInput <- writeExecutableString -< (compileScript, [relfile|compile.sh|])
    compiled <- compileDocker -< (cInput, scriptInput)
    returnA -< compiled :</> [relfile|out.o|]
  where
    compileScript =
      "#!/usr/bin/env bash\n\
      \gcc -c -o $2 $1\n"
    compileDocker = docker $ \(cInput, scriptInput) -> Docker.Config
      { Docker.image = "gcc"
      , Docker.optImageID = Just "7.3.0"
      , Docker.command = contentParam scriptInput
      , Docker.args = [contentParam cInput, textParam "/output/out.o"]
      , Docker.env = EnvExplicit []
      , Docker.stdout = NoOutputCapture
      }

-- | This flow takes a list of files which are assumed to be 'C' modules.
--   It uses an external step to compile those modules into an executable.
compileExec :: SimpleFlow [Content File] (Content File)
compileExec = proc mods -> do
    scriptInput <- writeExecutableString -< (compileScript, [relfile|compile.sh|])
    compiled <- compileDocker -< (mods, scriptInput)
    returnA -< compiled :</> [relfile|out|]
  where
    compileScript =
      "#!/usr/bin/env bash\n\
      \out=\"$1\"\n\
      \shift\n\
      \gcc -o \"$out\" $@\n"
    compileDocker = docker $ \(cModules, scriptInput) -> Docker.Config
      { Docker.image = "gcc"
      , Docker.optImageID = Just "7.3.0"
      , Docker.command = contentParam scriptInput
      , Docker.args = textParam "/output/out" : map contentParam cModules
      , Docker.env = EnvExplicit []
      , Docker.stdout = NoOutputCapture
      }

-- | This flow takes a file which is assumed to be an executable,
-- and a list of strings that are arguments for the executable.
-- It uses an external step to run the executable with the given arguments.
-- The output is stored in the file @out@ in the returned item.
runExec :: SimpleFlow (Content File, [String]) CS.Item
runExec = proc (exec, args) -> do
    scriptInput <- writeExecutableString -< (runScript, [relfile|run.sh|])
    runDocker -< (scriptInput, exec, args)
  where
    runScript =
      "#!/bin/sh\n\
      \toExec=\"$1\"\n\
      \shift\n\
      \\"$toExec\" $@ > /output/out\n"
    runDocker = docker $ \(script, exec, args) -> Docker.Config
      { Docker.image = "gcc"
      , Docker.optImageID = Just "7.3.0"
      , Docker.command = contentParam script
      , Docker.args = contentParam exec : map stringParam args
      , Docker.env = EnvExplicit []
      , Docker.stdout = NoOutputCapture
      }
