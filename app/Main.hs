module Main (main) where

import AST
import Data.Aeson.BetterErrors (ParseError)
import qualified Lib
import System.Environment
import System.IO

execute :: Either (ParseError String) AST -> IO ()
execute (Right s) = print (show s)
execute (Left err) = print (show err)

main :: IO ()
main = do
  file1 <- getArgs
  case file1 of
    [] -> putStrLn "file not found ☹️"
    _ -> do
      f <- openFile (head file1) ReadMode
      content <- hGetContents f
      Lib.execute $ Lib.parseToAST content
