{-# LANGUAGE OverloadedStrings #-}

module Parser.Header where

import Model.Header
import Parser.General
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.ParserCombinators.ReadP (many1)

headerParser :: Parser Header
headerParser = do
    _ <- lexeme $ string "namespace"
    name <- namespaceParser
    desc <- optional descriptionParser
    _ <- lexeme $ string "version"
    vers <- between (char '\"') (char '\"') (many (letterChar <|> char '.' <|> char '$' <|> digitChar))
    imports <- many importParser
    return $ MakeHeader name desc vers imports

importParser :: Parser String
importParser = do
    _ <- lexeme $ string "import"
    namespaceParser

namespaceParser :: Parser String
namespaceParser = many (letterChar <|> digitChar <|> char '.' <|> char '*')