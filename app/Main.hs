module Main (main) where

import qualified Interpreter
import qualified Parser

import System.Environment
import System.IO

main :: IO ()
main = do
  file1 <- getArgs
  case file1 of
    [] -> putStrLn "file not found ☹️"
    _ -> do
      f <- openFile (head file1) ReadMode
      content <- hGetContents f
      Interpreter.execute $ Parser.parseToAST content
