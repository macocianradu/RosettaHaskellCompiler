{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Header where

import Model.Header
import PrettyPrinter.General
import Prettyprinter
import Data.Char

-- |Converts a Header into a haskell valid String
printHeader :: Header -> String
printHeader (MakeHeader name (Just description) _ imports) =
    show $ vcat ["module" <+> pretty (convertFirst name) <+> "where",
        enclose "{-" "-}" (pretty description), 
        emptyDoc,
        vcat (map printImport imports),
        emptyDoc]
printHeader (MakeHeader name Nothing _ imports) =
    show $ vcat ["module" <+> pretty (convertFirst name) <+> "where",
        emptyDoc,
        vcat (map printImport imports),
        emptyDoc] 

-- |Converts an import name into an import prettyprinter doc
printImport :: String -> Doc a
printImport name = "import" <+> pretty name

convertName :: String -> String
convertName [] = []
convertName (c:cs) 
    | c == '.' = c : convertFirst cs
    | otherwise = c : convertName cs

convertFirst :: String -> String
convertFirst [] = []
convertFirst (c:cs) = toUpper c : convertName cs