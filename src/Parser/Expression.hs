{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import Parser.General
import Model.Function
import qualified Data.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char


-- |Parses a complete Rosetta expression into an Expression type
expressionParser :: Parser Expression
expressionParser = 
    choice [ ifParser,
    try functionCallParser,
    eqParser]

--------------------------------------------
-- Command Structures ----------------------
--------------------------------------------

-- |Parses a function call in Rosetta into an Expression
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

-- |Parses an if statement in Rosetta into an Expression
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
    
-- |Parses an expression between parentheses in Rosetta into an Expression
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

--------------------------------------------
-- Terminals -------------------------------
--------------------------------------------

-- |Parses a list in Rosetta into an Expression
listParser :: Parser Expression
listParser =
    do
        _ <- lexeme $ char '['
        expressions <- many $ try (expressionParser >>= \ex -> lexeme $ char ',' >> return ex)
        lastExpr <- try expressionParser
        _ <- lexeme $ char ']'
        return $ List (expressions ++ [lastExpr])

-- |Parses a variable in Rosetta into an Expression
variableParser :: Parser Expression
variableParser =
    do
        var <- camelNameParser
        inner <- many innerVariableParser
        return $ Variable (var ++ concatMap ("->" ++) inner)

-- |Parses an inner variable (a -> b) in Rosetta into an Expression
innerVariableParser :: Parser String
innerVariableParser =
    do
        _ <- lexeme $ string "->"
        camelNameParser

-- |Parses an integer in Rosetta into an Expression
integerParser :: Parser Expression
integerParser =
    do
        nr <- lexeme $ some digitChar
        return $ Int nr
    
-- |Parses a real number in Rosetta into an Expression    
decimalParser :: Parser Expression
decimalParser =
    do
        nr <- some digitChar
        _ <- char '.'
        real <- lexeme $ many digitChar
        return $ Real $ nr ++ "." ++ real
      
-- |Parses a boolean in Rosetta into an Expression  
booleanParser :: Parser Expression
booleanParser =
    do
        bol <- lexeme (string "True" <|> string "False")
        return $ Boolean $ Text.unpack bol

-- |Parses the empty statement in Rosetta into an Expression
emptyParser :: Parser Expression
emptyParser = 
    do
        _ <- lexeme $ string "empty"
        return Empty

-- |Parses any of the terminal statements in Rosetta into an Expression
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

-- |Parses an prefix function statement in Rosetta into an Expression
prefixParser :: Parser Expression
prefixParser = 
    do
        op <- lexeme $ choice $ fmap (try . string . Text.pack) prefixOperators
        PrefixExp (Text.unpack op) <$> expressionParser
        
-- |List of prefix operators
prefixOperators :: [String]
prefixOperators = ["-", "not"]

-- |Parses an equality statement in Rosetta into an Expression
eqParser :: Parser Expression
eqParser =
    do
        s <- sumParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) eqFunctions
        case op of
            Left _ -> return s
            Right o -> eqParser >>= \ex -> return $ InfixExp (Text.unpack o) s ex

-- |The list of equality statements in Rosetta
eqFunctions :: [String]
eqFunctions = ["=", "<", "<=", ">", ">=", "<>", "all =", "all <>", "any =", "any <>"]

-- |Parses a sum statement in Rosetta into an Expression
sumParser :: Parser Expression
sumParser =
    do
        f <- factorParser
        op <- lexeme $ observing (char '+' <|> char '-')
        case op of
            Left _ -> return f
            Right o -> sumParser >>= \ex -> return $ reverseExpression $ InfixExp [o] f ex

-- |Parses a multiplication or division statement in Rosetta into an Expression
factorParser :: Parser Expression
factorParser = 
    do 
        p <- powerParser
        op <- lexeme $ observing (char '*' <|> char '/')
        case op of
            Left _ -> return p
            Right o -> factorParser >>= \ex -> return $ reverseExpression $ InfixExp [o] p ex

-- |Parses a boolean statement in Rosetta into an Expression
boolOpParser :: Parser Expression
boolOpParser = 
    do 
        p <- postfixParser
        op <- lexeme $ observing (string "or" <|> string "and")
        case op of
            Left _ -> return p
            Right o -> boolOpParser >>= \ex -> return $ InfixExp (Text.unpack o) p ex

-- |Parses a power statement in Rosetta into an Expression
powerParser :: Parser Expression
powerParser = 
    do
        p <- boolOpParser
        op <- lexeme $ observing $ char '^'
        case op of
            Left _ -> return p
            Right _ -> powerParser >>= \ex -> return $ InfixExp "^" p ex
     
-- |Parses a postfix function in Rosetta into an Expression       
postfixParser :: Parser Expression
postfixParser = 
    do
        t <- terminalParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) postfixFunctions
        case op of
            Left _ -> return t
            Right o -> return $ PostfixExp (Text.unpack o) t

-- |The list of existing postfix Rosetta functions
postfixFunctions :: [String]
postfixFunctions = ["exists", "is absent", "count", "only-element", "single exists", "multiple exists"]

--------------------------------------------
-- Auxiliary  ------------------------------
--------------------------------------------

-- |Reverses the order of operations for left-associative functions
reverseExpression :: Expression -> Expression
reverseExpression (InfixExp op t1 (InfixExp op2 t2 e))
    | precedence op == precedence op2 = InfixExp op2 (reverseExpression (InfixExp op t1 t2)) e
    | otherwise = InfixExp op t1 (InfixExp op2 t2 e)
reverseExpression e = e


-- |The precedence of existing infix functions (higher goes first)
precedence :: String -> Int
precedence "or" = 1
precedence "and" = 10
precedence "+" = 2
precedence "-" = 2
precedence "*" = 3
precedence "/" = 3
precedence "^" = 4
precedence _ = 100