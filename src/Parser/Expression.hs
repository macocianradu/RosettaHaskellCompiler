{-# LANGUAGE OverloadedStrings #-}

module Parser.Expression where

import Text.Megaparsec
import Text.Megaparsec.Char
import Model.Function
import Parser.General
import Data.Text
     
variableParser :: Parser Expression
variableParser =
    do
        var <- camelNameParser
        _ <- spaceConsumer
        return $ Variable var
   
integerParser :: Parser Expression
integerParser =
    do
        nr <- some digitChar
        _ <- spaceConsumer
        return $ Literal $ show nr
        
decimalParser :: Parser Expression
decimalParser =
    do
        nr <- some digitChar
        _ <- char '.'
        real <- many digitChar
        _ <- spaceConsumer
        return $ Literal $ show nr ++ "." ++ real
        
booleanParser :: Parser Expression
booleanParser =
    do
        bol <- string (pack "True") <|> string (pack "False")
        _ <- spaceConsumer
        return $ Literal $ unpack bol

listParser :: Parser Expression
listParser = 
    do
        _ <- char '['
        _ <- spaceConsumer
        expressions <- many $ try expressionList
        _ <- spaceConsumer
        lastExpr <- try expressionParser
        _ <- spaceConsumer
        _ <- char ']'
        _ <- spaceConsumer
        return $ ExpressionList (expressions ++ [lastExpr])
        where
            expressionList = 
                do
                    expr <- expressionParser
                    _ <- spaceConsumer
                    _ <- char ','
                    _ <- spaceConsumer
                    return expr

emptyParser :: Parser Expression
emptyParser = 
    do
        _ <- string "empty"
        _ <- spaceConsumer
        return $ Literal "empty"

literalParser :: Parser Expression
literalParser =
    do
        choice 
            [ booleanParser,
             try emptyParser,
             try decimalParser,
             try variableParser,
             integerParser
            ]
        
parensParser :: Parser Expression 
parensParser = 
    do
        _ <- char '('
        expr <- expressionParser
        _ <- char ')'
        return expr

ifElseParser :: Parser Expression
ifElseParser =
    do
        (IfSimple cond expr) <- simpleIfParser
        _ <- string "else"
        _ <- spaceConsumer
        expr2 <- expressionParser
        _ <- spaceConsumer
        return $ IfElse cond expr expr2

simpleIfParser :: Parser Expression
simpleIfParser = 
    do
        _ <- string "if"
        _ <- spaceConsumer
        condition <- expressionParser
        _ <- spaceConsumer
        _ <- string "then"
        _ <- spaceConsumer
        expr <- expressionParser
        _ <- spaceConsumer
        return $ IfSimple condition expr
  
andParser :: Parser Expression
andParser = 
    do
        (ex1, ex2) <- try $ binaryParser "and"
        return $ And ex1 ex2
        
orParser :: Parser Expression
orParser =
    do
        (ex1, ex2) <- try $ binaryParser "or"
        return $ Or ex1 ex2
        
subParser :: Parser Expression
subParser =
    do
        (ex1, ex2) <- try $ binaryParser "-"
        return $ Subtract ex1 ex2
        
notParser :: Parser Expression
notParser =
    do
        _ <- string "not"
        _ <- spaceConsumer
        ex <- expressionParser
        _ <- spaceConsumer
        return $ Not ex
        
binaryParser :: String -> Parser (Expression, Expression)
binaryParser op =
    do
        ex1 <- literalParser
        _ <- spaceConsumer
        _ <- string $ pack op
        _ <- spaceConsumer
        ex2 <- expressionParser
        _ <- spaceConsumer
        return (ex1, ex2)
        
expressionParser :: Parser Expression
expressionParser = choice 
    [parensParser,
    notParser,
    andParser,
    orParser,
    subParser,
    ifElseParser,
    simpleIfParser,
    literalParser,
    variableParser
    ]

testIfElse = parseTest expressionParser "if asd then 123 else 4"
testAnd = parseTest expressionParser "23 and 34 and 24 and a"
testOr = parseTest expressionParser "23 or 34 or 24 or a"
testMin = parseTest expressionParser "(a - b) - c"