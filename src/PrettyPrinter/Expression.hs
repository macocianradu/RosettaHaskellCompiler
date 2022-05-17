{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Expression where

import Model.Type
import Prettyprinter
import Semantic.ExpressionChecker(coercionIncluded)
import Utils.Utils

printExpression :: ExplicitExpression -> Doc a
printExpression ExplicitEmpty = "[]" 
printExpression (ExplicitVariable name coer) = printCoercion coer $ pretty name
printExpression (Value s coer) = printCoercion coer $ pretty s
printExpression (ExplicitKeyword k) = pretty k
printExpression (ExplicitParens ex) = "(" <> printExpression ex <> ")"
printExpression (ExplicitList ex)  = list [printExpression x | x <- ex]
printExpression (ExplicitPath ex1 ex2 returnCoerce) = printCoercion (returnCoercion ex1) (printExpression ex1) <+> "->" <+> printCoercion (returnCoercion ex2) (printExpression ex2)
printExpression (ExplicitFunction "exists" args returnCoerce) = printCoercion returnCoerce "isJust" <+> printCoercion (snd $ head args) (printExpression (fst $ head args))
printExpression (ExplicitFunction "is absent" args returnCoerce) = printCoercion returnCoerce "isNothing" <+> printCoercion (snd $ head args) (printExpression (fst $ head args))
printExpression (ExplicitFunction "single exists" args returnCoerce) = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression (fst $ head args)) <+> "==" <+> "1"
printExpression (ExplicitFunction "multiple exists" args returnCoerce) = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression (fst $ head args)) <+> ">" <+> "1" 
printExpression (ExplicitFunction "count" args returnCoerce) = printCoercion returnCoerce "length" <+> printCoercion (snd $ head args) (printExpression (fst $ head args))
-- Equality expressions
-- [a] a all = 
-- any <>
printExpression (ExplicitFunction "=" args returnCoerce) = printCoercion (snd $ head args) (printExpression (fst $ head args)) <+> "==" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args))
printExpression (ExplicitFunction "<>" args returnCoerce) = printCoercion (snd $ head args) (printExpression (fst $ head args)) <+> "/=" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args))
printExpression (ExplicitFunction "any =" args returnCoerce) = printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args)) <+> "`elem`" <+> printCoercion (snd $ head args) (printExpression (fst $ head args))
printExpression (ExplicitFunction "all <>" args returnCoerce) = printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args)) <+> "`notElem`" <+> printCoercion (snd $ head args) (printExpression (fst $ head args))
printExpression (ExplicitFunction "all =" args returnCoerce) = "all (Eq)" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args)) <+> printCoercion (snd $ head args) (printExpression (fst $ head args))
printExpression (ExplicitFunction "and" args returnCoerce) = printCoercion (snd $ head args) (printExpression (fst $ head args)) <+> "&&" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args))
printExpression (ExplicitFunction "or" args returnCoerce) = printCoercion (snd $ head args) (printExpression (fst $ head args)) <+> "||" <+> printCoercion (snd $ head $ tail args) (printExpression (fst $ head $ tail args))
printExpression (ExplicitFunction name args returnCoerce) = 
    if null printedArgs then pretty (uncapitalize name) 
    else  "(" <> pretty (uncapitalize name) <+> printCoercion returnCoerce (hsep printedArgs) <> ")"
    where printedArgs = zipWith printCoercion [c | (e,c) <- args] [printExpression e | (e, c) <- args]
printExpression (ExplicitIfSimple cond thenBlock returnCoercion) = 
    "if" <+> printCoercion (snd cond) (printExpression (fst cond)) <+> 
    "then" <+> printCoercion (snd thenBlock) (printExpression (fst thenBlock)) <+> 
    "else" <+> case MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion returnCoercion] (MakeCardinalityIdCoercion (Bounds (0, 0))) `coercionIncluded` returnCoercion of
        Left err -> error $ show err
        Right c -> printCoercion c emptyDoc
printExpression (ExplicitIfElse cond thenBlock elseBlock returnCoercion) = 
    "if" <+> printCoercion (snd cond) (printExpression (fst cond)) <+> 
    "then" <+> printCoercion (snd thenBlock) (printExpression (fst thenBlock)) <+> 
    "else" <+> printCoercion (snd elseBlock) (printExpression (fst elseBlock))

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