{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Header where

import Model.Header
import PrettyPrinter.General
import Prettyprinter
import Data.Char
import Utils.Utils

-- |Converts a Header into a haskell valid String
printHeader :: Header -> String
printHeader (MakeHeader name (Just description) _ imports) =
    show $ vcat ["module" <+> pretty (removePeriods name) <+> "where",
        enclose "{-" "-}" (pretty description), 
        emptyDoc,
        vcat (map printImport imports),
        emptyDoc]
printHeader (MakeHeader name Nothing _ imports) =
    show $ vcat ["module" <+> pretty (removePeriods name) <+> "where",
        emptyDoc,
        vcat (map printImport imports),
        emptyDoc] 

-- |Converts an import name into an import prettyprinter doc
printImport :: String -> Doc a
printImport name = "import" <+> pretty (removePeriods name)