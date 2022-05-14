{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Type where
  
import Prettyprinter
import PrettyPrinter.General
import Model.Type
import Utils.Utils

-- |Converts an EnumType into a haskell valid String
printType :: Type -> String
printType (MakeType name (MakeType super _ _ _ _) description attributes conditions) = printType (MakeType name (BasicType "Object") description (superToAttribute super:attributes) conditions)
printType (MakeType name (BasicType "Object") description attributes conditions) =
    show $ printDescription description (vcat [nest 4 $ vcat ("data" <+> pretty name <+> "=" <+> "Make" <> pretty name <+> "{": printAttributes name attributes ++ map printCondition conditions), "}", emptyDoc, emptyDoc])
printType (MakeType _ (BasicType _) _ _ _) = error "Can't extend basic types"
printType (BasicType name) = show $ pretty name
   
-- |Creates an attribute that accesses the super type
superToAttribute :: String -> TypeAttribute
superToAttribute typ = MakeTypeAttribute "super" (MakeType typ (BasicType "Object") Nothing [] []) (Bounds (1, 1)) (Just "Pointer to super class")  
   
-- |Converts a list of TypeAttributes into a list of haskell valid Docs 
printAttributes :: String -> [TypeAttribute] -> [Doc a]
printAttributes _ [] = []
printAttributes objName [at] = [printAttribute objName at]
printAttributes objName (at : ats) = (printAttribute objName at <> ",") : printAttributes objName ats
   
-- |Converts a TypeAttribute into a haskell valid Doc
printAttribute :: String -> TypeAttribute -> Doc a
printAttribute objName (MakeTypeAttribute name typ crd description) =
    printDescription description 
        (pretty (uncapitalize objName) <> pretty (capitalize name) <+> "::" <+> printCardinality (MakeTypeAttribute name typ crd description))

-- |Converts a Cardinality into a haskell valid Doc
printCardinality :: TypeAttribute -> Doc a
printCardinality (MakeTypeAttribute _ typ (Bounds (x, y)) _)
    | x == 0 && y == 1 = "Maybe" <+> pretty (typeName typ)
    | x == 1 && y == 1 = pretty (typeName typ)
    | otherwise = "[" <> pretty (typeName typ) <> "]"
printCardinality (MakeTypeAttribute _ typ (OneBound _) _) = "[" <> pretty (typeName typ) <> "]"

printCondition :: Condition -> Doc a
printCondition (MakeCondition desc e) = printDescription desc ("--" <+> pretty (show e))