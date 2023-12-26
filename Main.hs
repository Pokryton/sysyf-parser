module Main where

import System.Environment

import Parser

main  :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Usage: ./sysyf-parser <file>"
  else do
    let filename = head args
    contents <- readFile filename
    case parseCompUnit filename contents of
      Left err -> print err
      Right ex -> mapM_ print ex
