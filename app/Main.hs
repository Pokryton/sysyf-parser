module Main where

import Data.Either
import System.Environment
import Text.Show.Pretty

import Parser

main  :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Usage: ./sysyf-parser <file>"
  else do
    res <- parseFile $ head args
    either print pPrint res
