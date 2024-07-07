module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )

import AbsGrammar
import ParGrammar
import TypeChecker
import Interpreter

import System.IO (hPutStrLn, stderr)

runFile :: String -> IO ()
runFile fileName = readFile fileName >>= run


handleParseError :: String -> IO ()
handleParseError err = do
    hPutStrLn stderr $ "\nParse Failed..."
    hPutStrLn stderr err
    exitFailure


executeProgram :: Program -> IO ()
executeProgram program = do
    result <- TypeChecker.runTypeCheck program
    case result of
        Left err -> do
            hPutStrLn stderr $ show err
            exitFailure
        Right _ -> Interpreter.runProgram program


run :: String -> IO ()
run input = case pProgram $ myLexer input of
    Left err -> handleParseError err
    Right program -> executeProgram program

usage :: IO ()
usage = do
    hPutStrLn stderr $ unlines
        [
        "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (no arguments)  Intepret a program from stdin.",
        "  (file)          Intepret a program from the given file."
        ]
    exitFailure


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"]  -> usage
        []          -> getContents >>= run
        [file]      -> runFile file
        _           -> usage