{-# LANGUAGE OverloadedStrings #-}

module Parser.General where

import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text

type Parser = Parsec Void Text

-- |Auxiliary parser that eliminates trailing white space
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- |Auxiliary parser that runs a parser and eliminates trailing white space 
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- |Parses a description in Rosetta into a String
descriptionParser :: Parser String
descriptionParser = 
    do
        _ <- string "<\""
        lexeme $ anySingle `manyTill` string "\">"
        
-- |Parses a pascal case name into a String (PascalCase)
pascalNameParser :: Parser String
pascalNameParser =
    do
        first <- upperChar
        rest <- lexeme $ many allowedChars
        if first:rest `notElem` restrictedNames then return (first:rest) else fail ((first:rest) ++ " is a restricted name")

-- |Parses a camel case name into a String (camelCase)
camelNameParser :: Parser String
camelNameParser =
    do
        first <- lowerChar
        rest <- lexeme $ many allowedChars
        if first:rest `notElem` restrictedNames then return (first:rest) else fail ((first:rest) ++ " is a restricted name")

-- |Parses any name that starts with a letter or '_' into a String
nameParser :: Parser String
nameParser = 
    do 
        first <- letterChar <|> char '_'
        rest <- lexeme $ many allowedChars
        if first:rest `notElem` restrictedNames then return (first:rest) else fail ((first:rest) ++ " is a restricted name")

-- |Parses a character allowed in names in Rosetta into a Char  
allowedChars :: Parser Char
allowedChars = letterChar <|> digitChar <|> char '_'
        
        
-- |List of restricted names used by Rosetta
restrictedNames :: [String]
restrictedNames = [
    "displayName",
    "enum",
    "func",
    "type",
    "extends",
    "inputs",
    "output"
  ]