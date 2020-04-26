module Expr (generateParseTree
            , infixToPrefix
            , getTokens) where

import System.IO (putStr, print, putStrLn, putChar)
import GHC.Unicode (isDigit, isAlpha, isSpace, isAlphaNum)


data ASTtree = ASTnode Token [ASTtree]
  deriving (Show, Eq)

data Token = TokenIdentifier String
  | TokenInteger Integer
  | TokenFloat Double
  | TokenArithmetic ArithmeticOp
  | TokenBit BitOp
  | TokenComparison ComparsionOp
  | TokenLogical LogicalOp
  | TokenAssignment AssignmentOp
  | TokenParen ParenOp
  deriving (Show, Eq)

data AssignmentOp =
  Assign | AssignPlus | AssignMinus | AssignDivide |
  AssignMultiply | AssignModulo | AssignExponent
  deriving (Show, Eq)

data ArithmeticOp = Plus | Minus | Divide | Multiply | Modulo | Exponent
  deriving (Show, Eq)

data BitOp = BitOr | BitAnd | BitNot
  deriving (Show, Eq)

data ComparsionOp = Less | LessEq | Great | GreatEq | Equal | NotEq
  deriving (Show, Eq)

data LogicalOp = LogicOr | LogicAnd
  deriving (Show, Eq)

data ParenOp = OpenParen | CloseParen
  deriving (Show, Eq)

getOperator :: String -> (Token, String)
getOperator [] = error "Invalid parse order"
getOperator (x:y:ys)
  | x == '<' && y == '=' = (TokenComparison LessEq, ys)
  | x == '>' && y == '=' = (TokenComparison GreatEq, ys)
  | x == '=' && y == '=' = (TokenComparison Equal, ys)
  | x == '!' && y == '=' = (TokenComparison NotEq, ys)

  | x == '+' && y == '=' = (TokenAssignment AssignPlus, ys)
  | x == '-' && y == '=' = (TokenAssignment AssignMinus, ys)
  | x == '*' && y == '=' = (TokenAssignment AssignMultiply, ys)
  | x == '/' && y == '=' = (TokenAssignment AssignDivide, ys)
  | x == '%' && y == '=' = (TokenAssignment AssignModulo, ys)
  | x == '^' && y == '=' = (TokenAssignment AssignExponent, ys)

  | x == '|' && y == '|' = (TokenLogical LogicOr, ys)
  | x == '&' && y == '&' = (TokenLogical LogicAnd, ys)
getOperator (x:xs)
  | x == '+' = (TokenArithmetic Plus, xs)
  | x == '-' = (TokenArithmetic Minus, xs)
  | x == '/' = (TokenArithmetic Divide, xs)
  | x == '*' = (TokenArithmetic Multiply, xs)
  | x == '%' = (TokenArithmetic Modulo, xs)
  | x == '^' = (TokenArithmetic Exponent, xs)

  | x == '|' = (TokenBit BitOr, xs)
  | x == '&' = (TokenBit BitAnd, xs)
  | x == '~' = (TokenBit BitNot, xs)

  | x == '<' = (TokenComparison Less, xs)
  | x == '>' = (TokenComparison Great, xs)
  | x == '=' = (TokenAssignment Assign, xs)
  | otherwise = error ("Invalid operator '" ++ [x] ++ "'")


extract pred str = extractInto "" str
  where
    extractInto parsed [] = (parsed, [])
    extractInto parsed (x:xs)
      | pred x =
        let (parse, rest) = extractInto parsed xs
        in (x:parse, rest)          -- Extract the identifier into a string
      | otherwise = (parsed, x:xs)  -- Return whatever we recieved


extractNum :: String -> (Token, String)
extractNum (x:xs) =
  let
    extractInto parsed [] gotPoint
      | gotPoint == 1 = error "Invalid position of point in float"
      | otherwise = (parsed, [])
    extractInto parsed (x:xs) gotPoint
      | isDigit x =
        let p = if gotPoint == 1 then 2 else 0
            (parse, rest) = extractInto parsed xs p
        in (x:parse, rest)          -- Extract the identifier into a string
      | x == '.' && gotPoint == 0 =
        let (parse, rest) = extractInto parsed xs 1
        in (x:parse, rest)
      | x == '.' && gotPoint /= 0 = error "Invalid postion of point in float"
      | otherwise = (parsed, x:xs)  -- Return whatever we recieved
    (value, rest) = extractInto "" (x:xs) 0
    token
      | elem '.' value = TokenFloat (read value::Double)
      | otherwise = TokenInteger (read value::Integer)
  in (token, rest)


getTokens :: String -> [Token]
getTokens "" = []
getTokens (x:xs)
  | elem x "/+=-%^<>!~*" =
    let (token, rest) = getOperator (x:xs)
    in [token] ++ (getTokens rest)
  | x == '(' = [TokenParen OpenParen] ++ (getTokens xs)
  | x == ')' = [TokenParen CloseParen] ++ (getTokens xs)
  | isAlpha x =
    let (parsed, rest) = extract isAlphaNum (x:xs)
    in [TokenIdentifier parsed] ++ (getTokens rest)
  | isDigit x =
    let (token, rest) = extractNum (x:xs)
    in [token] ++ (getTokens rest)
  | isSpace x = getTokens xs
  | otherwise = error "Unable to parse token"

isOperator :: Token -> Bool
isOperator (TokenInteger _) = False
isOperator (TokenFloat _) = False
isOperator (TokenIdentifier _) = False
isOperator (TokenAssignment _) = False
isOperator (TokenParen _) = False
isOperator _ = True

isUnaryOp :: Token -> Bool
isUnaryOp (TokenBit BitNot) = True
isUnaryOp _ = False

isBinaryOp :: Token -> Bool
isBinaryOp x = (isOperator x) && (not $ isUnaryOp x)

isParen :: Token -> Bool
isParen (TokenParen _) = True
isParen _ = False

isValue :: Token -> Bool
isValue (TokenInteger _) = True
isValue (TokenFloat _) = True
isValue (TokenIdentifier _) = True
isValue _ = False

isIdentifier :: Token -> Bool
isIdentifier (TokenIdentifier _) = True
isIdentifier _ = False

getPriority :: Token -> Int
getPriority (TokenAssignment _) = -1
getPriority (TokenParen CloseParen) = 0
getPriority (TokenLogical LogicOr) = 1
getPriority (TokenLogical LogicAnd) = 2
getPriority (TokenComparison Less) = 3
getPriority (TokenComparison Great) = 3
getPriority (TokenComparison LessEq) = 3
getPriority (TokenComparison GreatEq) = 3
getPriority (TokenComparison NotEq) = 3
getPriority (TokenComparison Equal) = 3
getPriority (TokenArithmetic Plus) = 4
getPriority (TokenArithmetic Minus) = 4
getPriority (TokenArithmetic Multiply) = 5
getPriority (TokenArithmetic Divide) = 5
getPriority (TokenArithmetic Modulo) = 6
getPriority (TokenArithmetic Exponent) = 7
getPriority (TokenBit BitOr) = 8
getPriority (TokenBit BitAnd) = 9
getPriority (TokenParen OpenParen) = 200
getPriority t
  | isUnaryOp t = 100
  | otherwise = error "Invalid operator"

toPopOp :: Token -> [Token] -> Bool
toPopOp (TokenParen CloseParen) _ = True
toPopOp _ [] = False
toPopOp (TokenParen OpenParen) _ = False
toPopOp _ ((TokenParen OpenParen):rest) = False
toPopOp (TokenBit BitNot) _ = False
toPopOp t (top:rest)
  | t == top = True
  | otherwise = getPriority t < getPriority top


infixToPrefix :: [Token] -> [Token]
infixToPrefix tokens = in2pre tokens
  where
    replaceParens x =
      if x == TokenParen OpenParen then
        TokenParen CloseParen
      else if x == TokenParen CloseParen then
        TokenParen OpenParen
      else x
    in2pre tokens = reverse (in2post (map replaceParens (reverse tokens)) [])
    in2post [] [] = []
    in2post [] (o:opStack)
      -- Sanity check. Normally this won't arise
      | o == TokenParen CloseParen = error "Close paren can't be on stack"
      -- reverse of what is the error
      | o == TokenParen OpenParen = error "No open paren found"
      | otherwise = o:(in2post [] opStack)
    in2post (t:rest) []
      | isValue t = t:(in2post rest [])
      -- reverse of what is the error
      | t == TokenParen CloseParen = error "No close paren was found."
      | (t == TokenParen OpenParen) || (isOperator t) = in2post rest (t:[])
      | otherwise = error ("Invalid token '" ++ show t ++ "' found.")
    in2post (t:rest) (o:opStack)
      | isValue t = t:(in2post rest (o:opStack))
      | t == TokenParen CloseParen =
        if o == TokenParen OpenParen then
          in2post rest opStack
        else if isOperator o then
          o:(in2post (t:rest) opStack)
        else
          error ("Invalid operator '" ++ show o ++ "'")
      | t == TokenParen OpenParen = in2post rest (t:o:opStack)
      | isOperator t =
        if toPopOp t (o:opStack) then
          o:(in2post rest (t:opStack))
        else
          in2post rest (t:o:opStack)
      | otherwise = error ("Invalid token '" ++ show t ++ "' found.")

generateParseTree :: [Token] -> ASTtree
generateParseTree tokens =
  let (value, _) = genParseTree tokens
      genParseTree (t:[])
        | isValue t = (ASTnode t [], [])
        | otherwise = error ("Invalid token '" ++ show t ++ "'")
      genParseTree (t:rest)
        | isUnaryOp t =
          let (val1, rest1) = genParseTree rest
          in (ASTnode t [val1], rest1)
        | isBinaryOp t =
          let (val1, rest1) = genParseTree rest
              (val2, rest2) = genParseTree rest1
          in (ASTnode t [val1, val2], rest2)
        | isValue t = (ASTnode t [], rest)
        | otherwise = error ("Invalid token '" ++ show t ++ "'")
  in value
