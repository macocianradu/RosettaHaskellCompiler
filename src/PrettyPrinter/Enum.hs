{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Enum where

import Model.Enum
import PrettyPrinter.General
import Prettyprinter
    
-- |Converts an EnumType into a haskell valid String
printEnum :: EnumType -> String
printEnum (MakeEnum name description values) =
    show $ printDescription description 
        (vcat ["data" <+> pretty name <+> "=", 
        indent 4 (printEnumValues name values), indent 4 "deriving (Eq)",
        "",
        printDisplayNames name values]) <> line <> line
    
-- |Converts a list of EnumValues into a haskell valid Doc
printEnumValues :: String -> [EnumValue] -> Doc a
printEnumValues _ [] = ""
printEnumValues enumName (x:xs) = vcat (printFirstEnumValue enumName x: map (printEnumValue enumName) xs)

-- |Converts the first EnumValue (in haskell without the '|') into a haskell valid Doc
printFirstEnumValue :: String -> EnumValue -> Doc a
printFirstEnumValue enumName (MakeEnumValue name description _) =
    printDescription description (pretty enumName <> pretty name)

-- |Converts a non-first EnumValue (in haskell with the '|') into a haskell valid Doc
printEnumValue :: String -> EnumValue -> Doc a
printEnumValue enumName (MakeEnumValue name description _) = 
    printDescription description ("|" <+> pretty enumName <> pretty name)
        
-- |Converts the display names of an EnumType into a haskell valid Doc
printDisplayNames :: String -> [EnumValue] -> Doc a
printDisplayNames name values = 
    nest 4 $ vcat ("instance Show" <+> pretty name <+> "where": map (printDisplayName name) values)
        
-- |Converts a single display name into a haskell valid Doc
printDisplayName :: String -> EnumValue -> Doc a
printDisplayName enumName (MakeEnumValue name _ (Just display)) =
    "show" <+> pretty enumName <> pretty name <+> "= \"" <> pretty display <> "\"" 
printDisplayName enumName (MakeEnumValue name _ Nothing) =
    "show" <+> pretty enumName <> pretty name <+> "= \"" <> pretty name <> "\""
        
        
