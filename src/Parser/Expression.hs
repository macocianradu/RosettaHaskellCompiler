{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import Parser.General
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
  
data Expression = Variable String
    | Int String
    | Real String
    | Boolean String
    | Empty
    | Parens Expression
    | List [Expression]
    | Function String [Expression]
    | PrefixExp String Expression
    | PostfixExp String Expression
    | InfixExp String Expression Expression
    | IfSimple Expression Expression
    | IfElse Expression Expression Expression
    deriving (Show)


expressionParser :: Parser Expression
expressionParser = 
    choice [ ifParser,
    try functionParser,
    eqParser]

--------------------------------------------
-- Command Structures ----------------------
--------------------------------------------

functionParser :: Parser Expression
functionParser =
    do
        f <- pascalNameParser
        _ <- spaceConsumer
        _ <- char '('
        ats <- many $ try (expressionParser >>= \ats -> spaceConsumer >> char ',' >> spaceConsumer >> return ats)
        lat <- optional expressionParser
        _ <- spaceConsumer
        _ <- char ')'
        _ <- spaceConsumer
        case lat of
            Nothing -> return $ Function f []
            Just at -> return $ Function f (ats ++ [at])

ifParser :: Parser Expression
ifParser =
    do
        _ <- string "if"
        _ <- spaceConsumer
        condition <- between (char '(') (char ')') expressionParser <|> expressionParser
        _ <- spaceConsumer
        _ <- string "then"
        _ <- spaceConsumer
        expr <- expressionParser
        _ <- spaceConsumer
        els <- observing $ string "else"
        _ <- spaceConsumer
        case els of
            Left _ -> return (IfSimple condition expr)
            Right _ -> expressionParser >>= \expr2 -> return (IfElse condition expr expr2)
    
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

--------------------------------------------
-- Terminals -------------------------------
--------------------------------------------

listParser :: Parser Expression
listParser =
    do
        _ <- char '['
        _ <- spaceConsumer
        expressions <- many $ try (expressionParser >>= \ex -> spaceConsumer >> char ',' >> spaceConsumer >> return ex)
        _ <- spaceConsumer
        lastExpr <- try expressionParser
        _ <- spaceConsumer
        _ <- char ']'
        _ <- spaceConsumer
        return $ List (expressions ++ [lastExpr])

variableParser :: Parser Expression
variableParser =
    do
        var <- camelNameParser
        _ <- spaceConsumer
        inner <- many innerVariableParser
        return $ Variable (var ++ concatMap ("->" ++) inner)

innerVariableParser :: Parser String
innerVariableParser =
    do
        _ <- string "->"
        _ <- spaceConsumer
        var <- camelNameParser
        _ <- spaceConsumer
        return var

integerParser :: Parser Expression
integerParser =
    do
        nr <- some digitChar
        _ <- spaceConsumer
        return $ Int $ show nr
        
decimalParser :: Parser Expression
decimalParser =
    do
        nr <- some digitChar
        _ <- char '.'
        real <- many digitChar
        _ <- spaceConsumer
        return $ Real $ show nr ++ "." ++ real
        
booleanParser :: Parser Expression
booleanParser =
    do
        bol <- string "True" <|> string "False"
        _ <- spaceConsumer
        return $ Boolean $ Text.unpack bol

emptyParser :: Parser Expression
emptyParser = 
    do
        _ <- string "empty"
        _ <- spaceConsumer
        return Empty

terminalParser :: Parser Expression
terminalParser =
    do
        choice 
            [ prefixParser,
             parens expressionParser >>= \e -> return (Parens e),
             listParser,
             try booleanParser,
             try emptyParser,
             try decimalParser,
             try variableParser,
             integerParser
            ]

--------------------------------------------
-- Expressions -----------------------------
--------------------------------------------

prefixParser :: Parser Expression
prefixParser = 
    do
        op <- choice $ fmap (try . string . Text.pack) prefixOperators
        _ <- spaceConsumer
        ex <- expressionParser
        _ <- spaceConsumer
        return $ PrefixExp (Text.unpack op) ex

eqParser :: Parser Expression
eqParser =
    do
        s <- sumParser
        _ <- spaceConsumer
        op <- observing (string  "<=" <|> string "=" <|> string "<" <|> string ">" <|> string ">=")
        _ <- spaceConsumer
        case op of
            Left _ -> return s
            Right o -> eqParser >>= \ex -> return $ InfixExp (Text.unpack o) s ex

sumParser :: Parser Expression
sumParser =
    do
        f <- factorParser
        _ <- spaceConsumer
        op <- observing (char '+' <|> char '-')
        _ <- spaceConsumer
        case op of
            Left _ -> return f
            Right o -> sumParser >>= \ex -> return $ reverseExpression $ InfixExp [o] f ex

factorParser :: Parser Expression
factorParser = 
    do 
        p <- powerParser
        _ <- spaceConsumer
        op <- observing (char '*' <|> char '/')
        _ <- spaceConsumer
        case op of
            Left _ -> return p
            Right o -> factorParser >>= \ex -> return $ reverseExpression $ InfixExp [o] p ex

powerParser :: Parser Expression
powerParser = 
    do
        p <- postfixParser
        _ <- spaceConsumer
        op <- observing $ char '^'
        _ <- spaceConsumer
        case op of
            Left _ -> return p
            Right _ -> powerParser >>= \ex -> return $ InfixExp "^" p ex
            
postfixParser :: Parser Expression
postfixParser = 
    do
        t <- terminalParser
        _ <- spaceConsumer
        op <- observing (string "exists" <|> string "is absent" <|> string "count" <|> string "only-element")
        _ <- spaceConsumer
        case op of
            Left _ -> return t
            Right o -> return $ PostfixExp (Text.unpack o) t

--------------------------------------------
-- Auxiliary  ------------------------------
--------------------------------------------

reverseExpression :: Expression -> Expression
reverseExpression (InfixExp op t1 (InfixExp op2 t2 e))
    | precedence op == precedence op2 = InfixExp op2 (reverseExpression (InfixExp op t1 t2)) e
    | otherwise = InfixExp op t1 (InfixExp op2 t2 e)
reverseExpression e = e

precedence :: String -> Int
precedence "or" = 1
precedence "and" = 1
precedence "+" = 2
precedence "-" = 2
precedence "*" = 3
precedence "/" = 3
precedence "^" = 4
precedence _ = 100

prefixOperators :: [String]
prefixOperators = ["-", "not"]

testArith3 = parseTest expressionParser "1 + (2 - 3)"
testArith4 = parseTest expressionParser "a * b - c * d - e * f = g * h - i * j - k * l"
testArith5 = parseTest expressionParser "a + b - c * d ^ e"
testArith6 = parseTest expressionParser "1 - 2 - 3 - 4 - 5 - 6"
testList = parseTest expressionParser "[1, 2, 3]"
testList2 = parseTest expressionParser "[1, 2 + 3, e]"
testFun = parseTest functionParser "Function()"
testFun2 = parseTest functionParser "Function(e)"
testFun3 = parseTest functionParser "Function(3, 3+2,e)"
testIf = parseTest expressionParser "if (Function(2 + 3, e)) then a + b - c * d ^ e -> x else not a exists"
testEverything = parseTest expressionParser "if [1, Function(3)] then 1 - 2 - 3 * a -> b ^ c"
testFail = parseTest expressionParser "if[1,as]thenxandoelseaora"