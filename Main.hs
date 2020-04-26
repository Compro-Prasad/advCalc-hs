module Main(main) where

import Expr

testInfixToPrefix [] = do
  print "Ends here"
testInfixToPrefix (expr:exprs) = do
  putStr (expr ++ ": ")
  print (generateParseTree $ infixToPrefix $ getTokens expr)
  testInfixToPrefix exprs

main = do
  testInfixToPrefix ["3", "3+2", "3+10/2", "3/2-5", "3/2-5/4", "3/(2-5)/4", "3/(2-5/4)"]
  print $ generateParseTree $ infixToPrefix $ getTokens "3 2"
