{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import Parser.General
import Model.Function
import Model.Type (Expression (..))
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
        condition <- lexeme $ expressionParser <|> between (char '(') (char ')') expressionParser
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

listOperations :: [String]
listOperations = ["map", "filter", "reduce"]

-- |Parses a variable in Rosetta into an Expression
variableParser :: Parser Expression
variableParser =
    do
        Variable <$> camelNameParser

enumValueParser :: Parser Expression
enumValueParser =
    do
        enum <- pascalNameParser
        _ <- lexeme $ string "->"
        Enum enum <$> pascalNameParser

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
            [
             try keywordParser,
             prefixParser,
             parens expressionParser >>= \e -> return (Parens e),
             listParser,
             try booleanParser,
             try emptyParser,
             try decimalParser,
             try variableParser,
             try enumValueParser,
             integerParser
            ]

--------------------------------------------
-- Expressions -----------------------------
--------------------------------------------
keywords :: [String]
keywords = ["one-of"]

keywordParser :: Parser Expression
keywordParser =
    do
        word <- lexeme $ choice $ fmap (try . string . Text.pack) keywords
        return $ Keyword $ Text.unpack word

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
eqFunctions = ["=", "<", "<=", ">", ">=", "<>"]

-- |Parses a sum statement in Rosetta into an Expression
sumParser :: Parser Expression
sumParser =
    do
        f <- factorParser
        op <- lexeme $ observing (string "+" <|> string "- ")
        case op of
            Left _ -> return f
            Right o -> sumParser >>= \ex -> return $ reverseExpression $ InfixExp (Text.unpack o) f ex

-- |Parses a multiplication or division statement in Rosetta into an Expression
factorParser :: Parser Expression
factorParser =
    do
        p <- powerParser
        op <- lexeme $ observing (char '*' <|> char '/')
        case op of
            Left _ -> return p
            Right o -> factorParser >>= \ex -> return $ reverseExpression $ InfixExp [o] p ex

-- |Parses a power statement in Rosetta into an Expression
powerParser :: Parser Expression
powerParser =
    do
        p <- boolOpParser
        op <- lexeme $ observing $ char '^'
        case op of
            Left _ -> return p
            Right _ -> powerParser >>= \ex -> return $ InfixExp "^" p ex

-- |Parses a boolean statement in Rosetta into an Expression
boolOpParser :: Parser Expression
boolOpParser =
    do
        p <- pathExpressionParser
        op <- lexeme $ observing (string "or" <|> string "and")
        case op of
            Left _ -> return p
            Right o -> boolOpParser >>= \ex -> return $ InfixExp (Text.unpack o) p ex

-- |Parses a postfix function in Rosetta into an Expression       
postfixParser :: Parser Expression
postfixParser =
    do
        t <- listUnaryOpParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) postfixFunctions
        case op of
            Left _ -> return t
            Right o -> return $ PostfixExp (Text.unpack o) t


listUnaryOperations :: [String]
listUnaryOperations = ["sum", "max", "flatten", "min", "join", "only-element", "count", "first", "last", "sort", "distinct"]

-- |Parses a simple operation on a list
listUnaryOpParser :: Parser Expression
listUnaryOpParser =
    do
        lst <- listOpParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) listUnaryOperations
        case op of
            Left er -> return lst
            Right o -> 
                do
                exp <- nestedPostOp lst
                return $ reverseExpression $ ListUnaryOp (Text.unpack o) exp

-- |Parses an operation on a list
listOpParser :: Parser Expression
listOpParser =
    do
        lst <- terminalParser
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) listOperations
        case op of
            Left _ -> return lst
            Right o ->                 
                do
                    con <- between (lexeme $ char '[') (lexeme $ char ']') expressionParser 
                    exp <- nestedPostOp lst
                    return $ reverseExpression $ ListOp (Text.unpack o) exp con

-- |Parses nested post operations on lists                   
nestedPostOp :: Expression -> Parser Expression
nestedPostOp ex =
    do
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) listUnaryOperations
        case op of
            Left er -> nestedListOp ex
            Right o -> 
                do
                exp <- nestedPostOp ex 
                return $ reverseExpression $ ListUnaryOp (Text.unpack o) exp

-- |Parses nested normal operations on lists
nestedListOp :: Expression -> Parser Expression
nestedListOp ex =
    do
        op <- lexeme $ observing $ choice $ fmap (try . string . Text.pack) listOperations
        case op of
            Left er -> return ex
            Right o -> 
                do
                    con <- between (lexeme $ char '[') (lexeme $ char ']') expressionParser 
                    exp <- nestedPostOp ex
                    return $ reverseExpression $ ListOp (Text.unpack o) exp con


-- |Parses a path expression (a -> b) in Rosetta into an Expression
pathExpressionParser :: Parser Expression
pathExpressionParser =
    do
        var <- postfixParser
        op <- lexeme $ observing $ string "->"
        case op of
            Left _ -> return var
            Right _ -> pathExpressionParser >>= \ex -> return $ reverseExpression $ PathExpression var ex

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
reverseExpression (PathExpression e1 (PathExpression e2 e3)) = PathExpression (reverseExpression (PathExpression e1 e2)) e3
reverseExpression (PathExpression e1 (ListOp op ex2 cond)) = ListOp op cond (reverseExpression (PathExpression e1 ex2))
reverseExpression (PathExpression e1 (ListUnaryOp op ex2)) = ListUnaryOp op (reverseExpression (PathExpression e1 ex2))
reverseExpression (ListOp op1 (ListOp op2 ex2 con2) con1) = ListOp op2 (reverseExpression (ListOp op1 ex2 con1)) con2
reverseExpression (ListOp op1 (ListUnaryOp op2 ex2) con1) = ListUnaryOp op2 (reverseExpression (ListOp op1 ex2 con1))
reverseExpression (ListUnaryOp op1 (ListOp op2 ex2 con2)) = ListOp op2 (reverseExpression (ListUnaryOp op1 ex2)) con2
reverseExpression (ListUnaryOp op1 (ListUnaryOp op2 ex2)) = ListUnaryOp op2 (reverseExpression (ListUnaryOp op1 ex2))
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