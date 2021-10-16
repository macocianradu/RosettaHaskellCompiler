{-# LANGUAGE OverloadedStrings #-}

module Parser.General where

import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

descriptionParser :: Parser String
descriptionParser = 
    do
        _ <- string "<\""
        lexeme $ anySingle `manyTill` string "\">"
        

pascalNameParser :: Parser String
pascalNameParser =
    do
        first <- upperChar
        rest <- lexeme $ many (letterChar <|> digitChar <|> char '_')
        return (first : rest)

camelNameParser :: Parser String
camelNameParser =
    do
        first <- lowerChar
        rest <- lexeme $ many (letterChar <|> digitChar <|> char '_')
        return (first : rest)

nameParser :: Parser String
nameParser = 
    do 
        first <- letterChar <|> char '_'
        rest <- lexeme $ many (letterChar <|> digitChar <|> char '_')
        return (first:rest)