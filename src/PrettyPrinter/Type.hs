{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Type where
  
import Prettyprinter
import PrettyPrinter.General
import Model.Type

printType :: Type -> String
printType (MakeType name _ description attributes) =
    show $ printDescription description (vcat [nest 4 $ vcat("data" <+> pretty name <+> "=" <+> "Make" <> pretty name <+> "{": map printAttribute attributes), "}", ""])
printType (BasicType name) = show $ pretty name
   
printAttribute :: TypeAttribute -> Doc a
printAttribute (MakeTypeAttribute name typ crd description) =
    printDescription description 
        (pretty name <+> "::" <+> printCardinality (MakeTypeAttribute name typ crd description))   

printCardinality :: TypeAttribute -> Doc a
printCardinality (MakeTypeAttribute _ typ ExactlyOne _) = pretty (typeName typ)
printCardinality (MakeTypeAttribute _ typ OneOrMore _) = "[" <> pretty (typeName typ) <> "]"
printCardinality (MakeTypeAttribute _ typ ZeroOrMore _) = "[" <> pretty (typeName typ) <> "]"
printCardinality (MakeTypeAttribute _ typ ZeroOrOne _) = "Maybe" <+> pretty (typeName typ)