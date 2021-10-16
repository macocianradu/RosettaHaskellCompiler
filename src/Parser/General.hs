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

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

descriptionParser :: Parser String
descriptionParser = 
    do
        _ <- string "<\""
        description <- anySingle `manyTill` string "\">"
        _ <- spaceConsumer
        return description
        

pascalNameParser :: Parser String
pascalNameParser =
    do
        first <- upperChar
        rest <- many (letterChar <|> digitChar <|> char '_')
        _ <- spaceConsumer
        return (first : rest)

camelNameParser :: Parser String
camelNameParser =
    do
        first <- lowerChar
        rest <- many (letterChar <|> digitChar <|> char '_')
        _ <- spaceConsumer
        return (first : rest)

nameParser :: Parser String
nameParser = 
    do 
        first <- letterChar <|> char '_'
        rest <- many (letterChar <|> digitChar <|> char '_')
        _ <- spaceConsumer
        return (first:rest)
        
untilParser :: String -> Parser String
untilParser x = try (anySingle `manyTill` string (pack x))