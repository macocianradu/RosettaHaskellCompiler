{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Header where

import Model.Header
import PrettyPrinter.General
import Prettyprinter
import Data.Char
import Utils.Utils

-- |Converts a Header into a haskell valid String
printHeader :: FilePath -> Header -> String
printHeader path (MakeHeader name (Just description) _ imports) =
    show $ vcat ["module" <+> pretty (removeChar path '-') <+> "where",
        enclose "{-" "-}" (pretty description), 
        emptyDoc,
        "import" <+> "Data.List",
        "import" <+> "Data.Maybe",
        vcat (map printImport imports),
        emptyDoc]
printHeader path (MakeHeader name Nothing _ imports) =
    show $ vcat ["module" <+> pretty (removeChar path '-')  <+> "where",
        emptyDoc,
        "import" <+> "Data.List",
        "import" <+> "Data.Maybe",
        vcat (map printImport imports),
        emptyDoc] 

-- |Converts an import name into an import prettyprinter doc
printImport :: String -> Doc a
printImport name = "import" <+> pretty (removeChar name '.')