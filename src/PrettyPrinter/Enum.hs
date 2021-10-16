{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Enum where

import Model.Enum
import PrettyPrinter.General
import Prettyprinter
    
printEnum :: EnumType -> String
printEnum (MakeEnum name description values) =
    show $ printDescription description 
        (vcat ["data" <+> pretty name <+> "=", 
        indent 4 (printEnumValues values),
        "",
        printDisplayNames name values])
    
printEnumValues :: [EnumValue] -> Doc a
printEnumValues [] = ""
printEnumValues (x:xs) = vcat (printFirstEnumValue x: map printEnumValue xs)

printFirstEnumValue :: EnumValue -> Doc a
printFirstEnumValue (MakeEnumValue name description _) =
    printDescription description (pretty name)

printEnumValue :: EnumValue -> Doc a
printEnumValue (MakeEnumValue name description _) = 
    printDescription description ("|" <+> pretty name)
        
printDisplayNames :: String -> [EnumValue] -> Doc a
printDisplayNames name values = 
    nest 4 $ vcat ("instance Show" <+> pretty name <+> "where": map printDisplayName values)
        
printDisplayName :: EnumValue -> Doc a
printDisplayName (MakeEnumValue name _ (Just display)) =
    "show" <+> pretty name <+> "= \"" <> pretty display <> "\"" 
printDisplayName (MakeEnumValue name _ Nothing) =
    "show" <+> pretty name <+> "= \"" <> pretty name <> "\""
        
        
