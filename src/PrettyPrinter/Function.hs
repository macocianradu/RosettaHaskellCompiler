{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Function where

import Prettyprinter
import Model.Function
import PrettyPrinter.General
import PrettyPrinter.Type
import Model.Type
  
-- show printStatementTree 

-- |Converts a Function into a haskell valid String
printFunction :: ExplicitFunction -> String
printFunction f = show $ vcat [printFunctionSignature (sign f), printFunctionBody f, emptyDoc]

-- |Converts the body of a Function into a haskell valid Doc
printFunctionBody :: ExplicitFunction -> Doc a
printFunctionBody (MakeExplicitFunction (MakeFunctionSignature name _ inp out) ex) = pretty name <+> printVariableNames inp <+> "=" <+> printExpression ex
printExpression :: ExplicitExpression -> Doc a
printExpression ExplicitEmpty = "[]" 
printExpression (ExplicitVariable name coer) = printCoercion coer $ pretty name
printExpression (Value s coer) = printCoercion coer $ pretty s
printExpression (ExplicitParens ex) = "(" <> printExpression ex <> ")"
printExpression (ExplicitList ex) = list (map printExpression ex)
printExpression (ExplicitFunction "exists" args returnCoerce) = printCoercion returnCoerce "isJust" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args)
printExpression (ExplicitFunction "is absent" args returnCoerce) = printCoercion returnCoerce "isNothing" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args)
printExpression (ExplicitFunction "single exists" args returnCoerce) = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args) <+> "==" <+> "1"
printExpression (ExplicitFunction "multiple exists" args returnCoerce) = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args) <+> ">" <+> "1" 
printExpression (ExplicitFunction "count" args returnCoerce) = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args)
-- Equality expressions
-- [a] a all = 
-- any <>
printExpression (ExplicitFunction "=" args returnCoerce) = printCoercion (snd $ head args) (printExpression $ fst $ head args) <+> "==" <+> printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args)
printExpression (ExplicitFunction "<>" args returnCoerce) = printCoercion (snd $ head args) (printExpression $ fst $ head args) <+> "/=" <+> printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args)
printExpression (ExplicitFunction "any =" args returnCoerce) = printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args)<+> "`elem`" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args)
printExpression (ExplicitFunction "all <>" args returnCoerce) = printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args)<+> "`notElem`" <+> printCoercion (snd $ head args) (printExpression $ fst $ head args)
printExpression (ExplicitFunction "all =" args returnCoerce) = "all (Eq)" <+> printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args) <+> printCoercion (snd $ head args) (printExpression $ fst $ head args)
printExpression (ExplicitFunction "and" args returnCoerce) = printCoercion (snd $ head args) (printExpression $ fst $ head args) <+> "&&" <+> printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args)
printExpression (ExplicitFunction "or" args returnCoerce) = printCoercion (snd $ head args) (printExpression $ fst $ head args) <+> "||" <+> printCoercion (snd $ head $ tail args) (printExpression $ fst $ head $ tail args)
printExpression (ExplicitFunction name args returnCoerce) = pretty name <+> tupled (zipWith printCoercion [c | (e,c) <- args] [printExpression e | (e, c) <- args])
printExpression (ExplicitIfSimple cond thenBlock returnCoercion) = "if" <+> printCoercion (snd cond) (printExpression $ fst cond) <+> "then" <+> printCoercion (snd thenBlock) (printExpression $ fst thenBlock) <+> "else" <+> "Nothing"
printExpression (ExplicitIfElse cond thenBlock elseBlock returnCoercion) = "if" <+> printCoercion (snd cond) (printExpression $ fst cond) <+> "then" <+> printCoercion (snd thenBlock) (printExpression $ fst thenBlock) <+> "else" <+> printCoercion (snd elseBlock) (printExpression $ fst elseBlock)

-- |Converts a coercion into a haskell string
printCoercion :: Coercion -> Doc a -> Doc a
printCoercion (MakeCoercion [] crd) d = printCardinalityCoercion crd d
printCoercion (MakeCoercion (t: ts) crd) d = printTypeCoercion t <+> printCoercion (MakeCoercion ts crd) d

printCardinalityCoercion :: CardinalityCoercion -> Doc a -> Doc a
printCardinalityCoercion (MakeCardinalityIdCoercion _) d = d
printCardinalityCoercion (MakeListCardinalityCoercion _ _) d = d
printCardinalityCoercion (MakeNothing2MaybeCoercion _ _) d = "Nothing"
printCardinalityCoercion (MakeNothing2ListCoercion _ _) d = "[]"
printCardinalityCoercion (MakeMaybe2ListCoercion _ _) d = "maybeToList" <+> d
printCardinalityCoercion (MakeObject2MaybeCoercion _ _) d = "Just" <+> d
printCardinalityCoercion (MakeObject2ListCoercion _ _) d = "[" <> d <> "]"

printTypeCoercion :: TypeCoercion -> Doc a
printTypeCoercion (MakeIdCoercion _) = emptyDoc
printTypeCoercion (MakeSuperCoercion _ _) = "super"
printTypeCoercion (MakeTypeCoercion _ _ t) = pretty t

-- |Converts a list of type attributes to a Doc with a list of variable names
printVariableNames :: [TypeAttribute] -> Doc a
printVariableNames vars = foldl (<+>) emptyDoc (map (pretty . attributeName) vars)

-- |Converts a function into a haskell valid Doc representing the signature of the function
printFunctionSignature :: FunctionSignature -> Doc a
printFunctionSignature (MakeFunctionSignature name description inputs output) =
    printDescription description (pretty name <+> prettyPrintType (Prelude.map printCardinality (inputs ++ [output])))

-- |Zips the signature with the needed characters ('::', '->')
prettyPrintType :: [Doc x] -> Doc x
prettyPrintType = align . sep . Prelude.zipWith (<+>) ("::" : repeat "->")

-- |Gets the cardinality of a variable by name
getVarCardinality :: [TypeAttribute] -> String -> Cardinality
getVarCardinality [] _ = error "Variable not a parameter" 
getVarCardinality (MakeTypeAttribute name _ card _ : inps) varName 
    | name == varName = card
    | otherwise = getVarCardinality inps varName