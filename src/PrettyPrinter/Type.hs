{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Type where
  
import Prettyprinter
import PrettyPrinter.General
import Model.Type

printType :: Type -> String
printType (MakeType name _ description attributes) =
    show $ printDescription description (vcat [nest 4 $ vcat ("data" <+> pretty name <+> "=" <+> "Make" <> pretty name <+> "{": map printAttribute attributes), "}", ""])
printType (BasicType name) = show $ pretty name
   
printAttributes :: [TypeAttribute] -> [Doc a]
printAttributes [] = []
printAttributes [at] = [printAttribute at]
printAttributes (at : ats) = (printAttribute at <> ",") : printAttributes ats

   
printAttribute :: TypeAttribute -> Doc a
printAttribute (MakeTypeAttribute name typ crd description) =
    printDescription description 
        (pretty name <+> "::" <+> printCardinality (MakeTypeAttribute name typ crd description))   

printCardinality :: TypeAttribute -> Doc a
printCardinality (MakeTypeAttribute _ typ (Bounds (x, y)) _)
    | x == 0 && y == 1 = "Maybe" <+> pretty (typeName typ)
    | x == 1 && y == 1 = pretty (typeName typ)
    | otherwise = "[" <> pretty (typeName typ) <> "]"
printCardinality (MakeTypeAttribute _ typ NoBounds _) = "[" <> pretty (typeName typ) <> "]"
printCardinality (MakeTypeAttribute _ typ (OneBound _) _) = "[" <> pretty (typeName typ) <> "]"