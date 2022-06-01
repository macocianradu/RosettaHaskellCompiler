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
    show $ printTypeName name description <+>
    printAttributes name conditions attributes <> line <> line
printType (MakeType _ (BasicType _) _ _ _) = error "Can't extend basic types"
printType (BasicType name) = show $ pretty name
   
printTypeName :: String -> Maybe String -> Doc a
printTypeName name desc = printDescription desc ("data" <+> pretty name <+> "=") 

printDefault :: String -> String -> [TypeAttribute] -> Doc a
printDefault name cons ats = "make" <> pretty name <+> "=" <+> pretty cons <+> "{" <> sep (punctuate "," 
    ([pretty (uncapitalize name) <> pretty (capitalize (attributeName x)) <+> "=" <+> "Nothing" | x <- ats, cardinality x == Bounds (0, 1)] ++ 
    [pretty (uncapitalize name) <> pretty (capitalize (attributeName x)) <+> "=" <+> "[]" | x <- ats, lowerBound (cardinality x) == 0 && upperBound (cardinality x) > 1])) <> "}"

-- |Creates an attribute that accesses the super type
superToAttribute :: String -> TypeAttribute
superToAttribute typ = MakeTypeAttribute "super" (MakeType typ (BasicType "Object") Nothing [] []) (Bounds (1, 1)) (Just "Pointer to super class")  
   
-- |Converts a list of TypeAttributes into a list of haskell valid Docs 
printAttributes :: String -> [Condition] -> [TypeAttribute] -> Doc a
printAttributes objName conditions ats
    | MakeCondition Nothing (Keyword "one-of") `elem` conditions = vcat [nest 4 $ vcat $ 
        zipWith (<>) ("" : repeat "| ") (map (printSumType objName) (increaseBound ats)) ++ map printCondition conditions, " deriving (Eq)"]
    | length ats < 2 = vcat [nest 4 $ vcat $ 
        zipWith (<>) ("" : repeat "| ") (map (printSumType objName) ats) ++ map printCondition conditions, " deriving (Eq)", printDefault objName (objName ++ capitalize (attributeName (head ats))) ats]
    | otherwise = vcat [nest 4 $ vcat ("Make" <> pretty objName <+> "{" : 
        punctuate comma (map (printAttribute objName) ats) ++ map printCondition conditions), "}"] <+> "deriving (Eq)" <> line <> printDefault objName ("Make" ++ objName) ats
   
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

printSumType :: String -> TypeAttribute -> Doc a
printSumType objName (MakeTypeAttribute name typ crd d) = pretty objName <> pretty (capitalize name) <+> "{"<> pretty (uncapitalize objName) <> pretty (capitalize name) <+> "::" <+> printCardinality (MakeTypeAttribute name typ crd d) <> "}"

-- |Increase the lower bound when it is 0 to 1. Used for printing types with "one-of" condition
increaseBound :: [TypeAttribute] -> [TypeAttribute]
increaseBound [] = []
increaseBound (MakeTypeAttribute name typ (Bounds (0, b)) desc : ts) = MakeTypeAttribute name typ (Bounds (1, b)) desc : increaseBound ts
increaseBound (t : ts) = t : increaseBound ts