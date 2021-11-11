{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import Parser.General
import Model.Function
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char

expressionParser :: Parser Expression
expressionParser = 
    choice [ ifParser,
    try functionCallParser,
    eqParser]

--------------------------------------------
-- Command Structures ----------------------
--------------------------------------------

functionCallParser :: Parser Expression
functionCallParser =
    do
        f <- lexeme pascalNameParser
        _ <- lexeme $ char '('
        ats <- many $ try (expressionParser >>= \ats -> lexeme $ char ',' >> return ats)
        lat <- optional $ lexeme expressionParser
        _ <- lexeme $ char ')'
        case lat of
            Nothing -> return $ Function f []
            Just at -> return $ Function f (ats ++ [at])

ifParser :: Parser Expression
ifParser =
    do
        _ <- lexeme $ string "if"
        condition <- lexeme $ between (char '(') (char ')') expressionParser <|> expressionParser
        _ <- lexeme $ string "then"
        expr <- expressionParser
        els <- observing $ lexeme $ string "else"
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
        _ <- lexeme $ char '['
        expressions <- many $ try (expressionParser >>= \ex -> lexeme $ char ',' >> return ex)
        lastExpr <- try expressionParser
        _ <- lexeme $ char ']'
        return $ List (expressions ++ [lastExpr])

variableParser :: Parser Expression
variableParser =
    do
        var <- camelNameParser
        inner <- many innerVariableParser
        return $ Variable (var ++ concatMap ("->" ++) inner)

innerVariableParser :: Parser String
innerVariableParser =
    do
        _ <- lexeme $ string "->"
        camelNameParser

integerParser :: Parser Expression
integerParser =
    do
        nr <- lexeme $ some digitChar
        return $ Int nr
        
decimalParser :: Parser Expression
decimalParser =
    do
        nr <- some digitChar
        _ <- char '.'
        real <- lexeme $ many digitChar
        return $ Real $ nr ++ "." ++ real
        
booleanParser :: Parser Expression
booleanParser =
    do
        bol <- lexeme (string "True" <|> string "False")
        return $ Boolean $ Text.unpack bol

emptyParser :: Parser Expression
emptyParser = 
    do
        _ <- lexeme $ string "empty"
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
        op <- lexeme $ choice $ fmap (try . string . Text.pack) prefixOperators
        PrefixExp (Text.unpack op) <$> expressionParser

eqParser :: Parser Expression
eqParser =
    do
        s <- sumParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) eqFunctions
        case op of
            Left _ -> return s
            Right o -> eqParser >>= \ex -> return $ InfixExp (Text.unpack o) s ex

eqFunctions :: [String]
eqFunctions = ["=", "<", "<=", ">", ">=", "<>", "all =", "all <>", "any =", "any <>"]

sumParser :: Parser Expression
sumParser =
    do
        f <- factorParser
        op <- lexeme $ observing (char '+' <|> char '-')
        case op of
            Left _ -> return f
            Right o -> sumParser >>= \ex -> return $ reverseExpression $ InfixExp [o] f ex

factorParser :: Parser Expression
factorParser = 
    do 
        p <- powerParser
        op <- lexeme $ observing (char '*' <|> char '/')
        case op of
            Left _ -> return p
            Right o -> factorParser >>= \ex -> return $ reverseExpression $ InfixExp [o] p ex

boolOpParser :: Parser Expression
boolOpParser = 
    do 
        p <- postfixParser
        op <- lexeme $ observing (string "or" <|> string "and")
        case op of
            Left _ -> return p
            Right o -> boolOpParser >>= \ex -> return $ InfixExp (Text.unpack o) p ex

powerParser :: Parser Expression
powerParser = 
    do
        p <- boolOpParser
        op <- lexeme $ observing $ char '^'
        case op of
            Left _ -> return p
            Right _ -> powerParser >>= \ex -> return $ InfixExp "^" p ex
            
postfixParser :: Parser Expression
postfixParser = 
    do
        t <- terminalParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) postfixFunctions
        case op of
            Left _ -> return t
            Right o -> return $ PostfixExp (Text.unpack o) t

postfixFunctions :: [String]
postfixFunctions = ["exists", "is absent", "count", "only-element", "single exists", "multiple exists"]

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