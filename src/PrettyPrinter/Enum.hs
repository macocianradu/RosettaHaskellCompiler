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
        indent 4 (printEnumValues values),
        "",
        printDisplayNames name values])
    
-- |Converts a list of EnumValues into a haskell valid Doc
printEnumValues :: [EnumValue] -> Doc a
printEnumValues [] = ""
printEnumValues (x:xs) = vcat (printFirstEnumValue x: map printEnumValue xs)

-- |Converts the first EnumValue (in haskell without the '|') into a haskell valid Doc
printFirstEnumValue :: EnumValue -> Doc a
printFirstEnumValue (MakeEnumValue name description _) =
    printDescription description (pretty name)

-- |Converts a non-first EnumValue (in haskell with the '|') into a haskell valid Doc
printEnumValue :: EnumValue -> Doc a
printEnumValue (MakeEnumValue name description _) = 
    printDescription description ("|" <+> pretty name)
        
-- |Converts the display names of an EnumType into a haskell valid Doc
printDisplayNames :: String -> [EnumValue] -> Doc a
printDisplayNames name values = 
    nest 4 $ vcat ("instance Show" <+> pretty name <+> "where": map printDisplayName values)
        
-- |Converts a single display name into a haskell valid Doc
printDisplayName :: EnumValue -> Doc a
printDisplayName (MakeEnumValue name _ (Just display)) =
    "show" <+> pretty name <+> "= \"" <> pretty display <> "\"" 
printDisplayName (MakeEnumValue name _ Nothing) =
    "show" <+> pretty name <+> "= \"" <> pretty name <> "\""
        
        
