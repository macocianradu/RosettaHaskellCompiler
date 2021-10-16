{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Type where
  
import Prettyprinter
import PrettyPrinter.General
import Model.Type
import Model.Enum

printType :: Type -> String
printType (MakeType name description attributes) =
    show $ printDescription description (vcat [nest 4 $ vcat("data" <+> pretty name <+> "=" <+> "Make" <> pretty name <+> "{": map printAttribute attributes), "}", ""])
printType x = show x 
   
printTypeName :: Type -> String
printTypeName (MakeType name _ _) = name
printTypeName (TypeFromBasicType name) = show name
printTypeName (TypeFromEnum (MakeEnum name _ _)) = name 
   
printAttribute :: TypeAttribute -> Doc a
printAttribute (MakeTypeAttribute name typ crd description) =
    printDescription description 
        (pretty name <+> "::" <+> printCardinality (MakeTypeAttribute name typ crd description))   

printCardinality :: TypeAttribute -> Doc a
printCardinality (MakeTypeAttribute _ typ ExactlyOne _) = pretty typ
printCardinality (MakeTypeAttribute _ typ OneOrMore _) = "[" <> pretty typ <> "]"
printCardinality (MakeTypeAttribute _ typ ZeroOrMore _) = "[" <> pretty typ <> "]"
printCardinality (MakeTypeAttribute _ typ ZeroOrOne _) = "Maybe" <+> pretty typ