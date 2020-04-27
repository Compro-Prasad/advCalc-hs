module Main(main) where


import Expr
import System.Exit
import System.IO


inputLoop func = do
  putStr "> "
  hFlush stdout
  input <- getLine
  process input
  where
    process input
      | input == "quit" = exitSuccess
      | otherwise = do
          putStrLn $ show $ func input
          inputLoop func


main = do
  inputLoop (generateParseTree . infixToPrefix . getTokens)
