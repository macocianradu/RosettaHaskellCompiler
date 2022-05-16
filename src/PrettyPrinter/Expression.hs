{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Expression where

import Model.Type
import Prettyprinter
import Semantic.ExpressionChecker(coercionIncluded)

printExpression :: ExplicitExpression -> Coercion -> Doc a
printExpression ExplicitEmpty _ = "[]" 
printExpression (ExplicitVariable name coer) out = case coer `coercionIncluded` out of 
    Left err -> error $ show coer ++ "//" ++ show out --err
    Right c -> printCoercion c $ pretty name
printExpression (Value s coer) out = case coer `coercionIncluded` out of
    Left err -> error $ show err 
    Right c -> printCoercion c $ pretty s
printExpression (ExplicitKeyword k) out = pretty k
printExpression (ExplicitParens ex) out = "(" <> printExpression ex out <> ")"
printExpression (ExplicitList ex) out = list [printExpression x out | x <- ex]
printExpression (ExplicitPath ex1 ex2 returnCoerce) out = printCoercion (returnCoercion ex1) (printExpression ex1 (returnCoercion ex1)) <+> "->" <+> printCoercion (returnCoercion ex2) (printExpression ex2 out)
printExpression (ExplicitFunction "exists" args returnCoerce) out = printCoercion returnCoerce "isJust" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out)
printExpression (ExplicitFunction "is absent" args returnCoerce) out = printCoercion returnCoerce "isNothing" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out)
printExpression (ExplicitFunction "single exists" args returnCoerce) out = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out) <+> "==" <+> "1"
printExpression (ExplicitFunction "multiple exists" args returnCoerce) out = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out) <+> ">" <+> "1" 
printExpression (ExplicitFunction "count" args returnCoerce) out = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out)
-- Equality expressions
-- [a] a all = 
-- any <>
printExpression (ExplicitFunction "=" args returnCoerce) out = printCoercion (snd $ head args) (printExpression (fst $ head args) out) <+> "==" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out)
printExpression (ExplicitFunction "<>" args returnCoerce) out = printCoercion (snd $ head args) (printExpression (fst $ head args) out) <+> "/=" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out)
printExpression (ExplicitFunction "any =" args returnCoerce) out = printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out) <+> "`elem`" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out)
printExpression (ExplicitFunction "all <>" args returnCoerce) out = printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out) <+> "`notElem`" <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out)
printExpression (ExplicitFunction "all =" args returnCoerce) out = "all (Eq)" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out) <+> printCoercion (snd $ head args) (printExpression (fst $ head args) out)
printExpression (ExplicitFunction "and" args returnCoerce) out = printCoercion (snd $ head args) (printExpression (fst $ head args) out) <+> "&&" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out)
printExpression (ExplicitFunction "or" args returnCoerce) out = printCoercion (snd $ head args) (printExpression (fst $ head args) out) <+> "||" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args) out)
printExpression (ExplicitFunction name args returnCoerce) out = pretty name <+> tupled (zipWith printCoercion [c | (e,c) <- args] [printExpression e out | (e, c) <- args])
printExpression (ExplicitIfSimple cond thenBlock returnCoercion) out = 
    "if" <+> printCoercion (snd cond) (printExpression (fst cond) (MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1))))) <+> 
    "then" <+> printCoercion (snd thenBlock) (printExpression (fst thenBlock) out) <+> 
    "else" <+> case MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion returnCoercion] (MakeCardinalityIdCoercion (Bounds (0, 0))) `coercionIncluded` out of
        Left err -> error $ show err
        Right c -> printCoercion c emptyDoc
printExpression (ExplicitIfElse cond thenBlock elseBlock returnCoercion) out = 
    "if" <+> printCoercion (snd cond) (printExpression (fst cond) (MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1))))) <+> 
    "then" <+> printCoercion (snd thenBlock) (printExpression (fst thenBlock) out) <+> 
    "else" <+> printCoercion (snd elseBlock) (printExpression (fst elseBlock) out)

-- |Converts a coercion into a haskell string
printCoercion :: Coercion -> Doc a -> Doc a
printCoercion (MakeCoercion [] crd) d = printCardinalityCoercion crd d
printCoercion (MakeCoercion (t: ts) crd) d = printTypeCoercion t <> printCoercion (MakeCoercion ts crd) d

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
printTypeCoercion (MakeSuperCoercion _ _) = "super" <+> emptyDoc
printTypeCoercion (MakeTypeCoercion _ _ t) = pretty t <+> emptyDoc

-- |Converts a list of type attributes to a Doc with a list of variable names
printVariableNames :: [TypeAttribute] -> Doc a
printVariableNames vars = foldl (<+>) emptyDoc (map (pretty . attributeName) vars)