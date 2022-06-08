{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Expression where

import Model.Type
import Prettyprinter
import Semantic.ExpressionChecker(coercionIncluded)
import Utils.Utils
import Data.List (isPrefixOf)

printExpression :: ExplicitExpression -> Doc a
printExpression ExplicitEmpty = "[]"
printExpression (ExplicitVariable name coer) = printCoercion coer $ pretty name
printExpression (Value "empty" coer) = error $ show coer --printCoercion coer "[]"
printExpression (Value s coer) = printCoercion coer $ pretty s
printExpression (ExplicitKeyword k) = pretty k
printExpression (ExplicitEnumCall name val coer) = printCoercion coer $ pretty name <> pretty val
printExpression (ExplicitParens ex c) = "(" <> printExpression ex <> ")"
printExpression (ExplicitList ex) = Prettyprinter.list [printExpression x | x <- ex]
printExpression (ExplicitReduce op lst v1 v2 cond coer) = "foldl1" <+> enclose "(" ")" ("\\" <+> pretty v1 <+> pretty v2 <+> "->" <+> printExpression cond) <+> enclose "(" ")" (printExpression lst)  
printExpression (ExplicitListOp op lst cond coer) = pretty op <+> enclose "(" ")" ("\\item" <+> "->" <+> printExpression cond) <> line <> enclose "(" ")" (printExpression lst)
--printExpression (ExplicitListOp "filter" lst cond coer) = "filter" <+> enclose "(" ")" ("\\x" <+> "->" <+> printExpression cond) <> line <> enclose "(" ")" (printExpression lst)
--printExpression (ExplicitListOp op lst cond coer) = pretty op <+> nest 4 (vsep [emptyDoc, enclose "(" ")" (printExpression cond), enclose "(" ")" (printExpression lst)])
printExpression (ExplicitListUnaryOp "only-element" lst coer)
    | toCardinality (cardinalityCoercion (returnCoercion lst)) == Bounds (0, 1) = "fromJust" <+> enclose "(" ")" (nest 4 (line <> printExpression lst))
    | otherwise = "head" <+> enclose "(" ")" (nest 4 (line <> printExpression lst))
printExpression (ExplicitListUnaryOp "flatten" lst coer) = "concat" <+> enclose "(" ")" (nest 4 (line <> printExpression lst))
printExpression (ExplicitListUnaryOp op lst coer) = pretty op <+> nest 4 (printExpression lst)
printExpression (ExplicitPath (ExplicitVariable var1 ret1) (ExplicitVariable var2 ret2) returnCoerce) = 
    pretty (uncapitalize $ typeName $ coercionType $ typeCoercion ret1) <> pretty (capitalize var2) <+> enclose "(" ")" (printCoercion ret1 (pretty var1))
printExpression (ExplicitPath ex1 (ExplicitVariable var ret) returnCoerce) =  
    pretty (uncapitalize $ typeName $ typeFromExpression ex1) <> pretty (capitalize var) <+> nest 4 (line <>
    enclose "(" ")" (printCoercion (returnCoercion ex1) (printExpression (ex1{returnCoercion = MakeCoercion [MakeIdCoercion (typeFromExpression ex1)] (MakeCardinalityIdCoercion (Bounds (1,1)))}))))
printExpression ExplicitPath {} = error "This should never happen. Path Expression 2nd argument is not variable"
printExpression (ExplicitFunction "exists" args returnCoerce) 
    | toCardinality (cardinalityCoercion (snd (head args))) == Bounds (0, 1) = printCoercion returnCoerce "isJust" <+> enclose "(" ")" (printCoercion (snd $ head args) (printExpression (fst $ head args)))
    | otherwise = printCoercion returnCoerce "not" <+> enclose "(" ")" ("null" <+> enclose "(" ")" (printCoercion (snd $ head args) (printExpression (fst $ head args))))
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
printExpression (ExplicitFunction name args returnCoerce) 
    | null printedArgs = pretty (uncapitalize name)
    | length printedArgs == 1 = pretty (uncapitalize name) <+> enclose "(" ")" (printCoercion (snd (head args)) (printExpression (fst (head args))))
    | otherwise = pretty (uncapitalize name) <+> hsep (map (enclose "(" ")") printedArgs)
    where printedArgs = zipWith printCoercion [c | (e,c) <- args] [printExpression e | (e, c) <- args]
printExpression (ExplicitIfSimple cond thenBlock returnCoercion) = 
    printIf
        (printCoercion (snd cond) (printExpression (fst cond)))
        (printCoercion (snd thenBlock) (printExpression (fst thenBlock)))
        (case MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion returnCoercion] (MakeCardinalityIdCoercion (Bounds (0, 0))) `coercionIncluded` returnCoercion of
            Left err -> error $ show err
            Right c -> printCoercion c emptyDoc)

printExpression (ExplicitIfElse (ExplicitFunction op e rc, c) thenBlock elseBlock _) 
    | op `elem` ["exists", "only exists"] = case isOneOf (fst (head e)) of
        Nothing -> printIf 
            (printCoercion c (printExpression  (ExplicitFunction op e rc))) 
            (printCoercion (snd thenBlock) (printExpression (fst thenBlock)))
            (printCoercion (snd elseBlock) (printExpression (fst elseBlock)))
        Just ex -> "case" <+> printExpression (fst ex) <+> "of" <+> nest 4 
            (line <> pretty (typeName (typeFromExpression (fst ex))) <> pretty (typeName (typeFromExpression (snd ex))) <+> "x" <+> "->" 
                <+> printCoercion (snd thenBlock) (printExpression (replace (fst (head e)) (fst thenBlock) "x")) <> line <>
                getCases elseBlock)
                 
    | otherwise = 
        printIf (printCoercion c (printExpression  (ExplicitFunction op e rc)))
            (printCoercion (snd thenBlock) (printExpression (fst thenBlock)))
            (printCoercion (snd elseBlock) (printExpression (fst elseBlock)))

printExpression (ExplicitIfElse cond thenBlock elseBlock _) = 
    printIf 
        (printCoercion (snd cond) (printExpression (fst cond))) 
        (printCoercion (snd thenBlock) (printExpression (fst thenBlock)))
        (printCoercion (snd elseBlock) (printExpression (fst elseBlock)))

printIf :: Doc a -> Doc a -> Doc a -> Doc a
printIf cond thenBlock elseBlock = "if" <+> cond <+> nest 4 ( line <>
        "then" <+> thenBlock <+> line <>
        "else" <+> elseBlock) 

-- |Converts a coercion into a haskell string
printCoercion :: Coercion -> Doc a -> Doc a
printCoercion (MakeCoercion [] crd) d = printCardinalityCoercion crd d
printCoercion (MakeCoercion (MakeIdCoercion _: ts) crd) d = printCoercion (MakeCoercion ts crd) d
printCoercion (MakeCoercion (MakeSuperCoercion _ _: ts) crd) d = d <> "Super" <+> printCoercion (MakeCoercion ts crd) d
printCoercion (MakeCoercion (MakeTypeCoercion {}: ts) crd) d = printCoercion (MakeCoercion ts crd) d

printCardinalityCoercion :: CardinalityCoercion -> Doc a -> Doc a
printCardinalityCoercion (MakeCardinalityIdCoercion _) d = d
printCardinalityCoercion (MakeListCardinalityCoercion _ _) d = d
printCardinalityCoercion (MakeNothing2MaybeCoercion _ _) d = "Nothing"
printCardinalityCoercion (MakeNothing2ListCoercion _ _) d = "[]"
printCardinalityCoercion (MakeMaybe2ListCoercion _ _) d = "maybeToList" <+> enclose "(" ")" d
printCardinalityCoercion (MakeObject2MaybeCoercion _ _) d = "Just" <+> enclose "(" ")" d
printCardinalityCoercion (MakeMaybe2ObjectCoercion _) d = "fromJust" <+> enclose "(" ")" d
printCardinalityCoercion (MakeObject2ListCoercion _ _) d = "[" <> d <> "]"
printCardinalityCoercion (MakeOneOfCoercion _) d = d

printTypeCoercion :: TypeCoercion -> Doc a -> Doc a
printTypeCoercion (MakeIdCoercion _) v = emptyDoc
printTypeCoercion (MakeSuperCoercion _ _) v = v <> "Super"
printTypeCoercion (MakeTypeCoercion _ _ t) v = pretty t

-- |Converts a list of type attributes to a Doc with a list of variable names
printVariableNames :: [TypeAttribute] -> Doc a
printVariableNames vars = foldl (<+>) emptyDoc (map (pretty . attributeName) vars)

isOneOf :: ExplicitExpression -> Maybe (ExplicitExpression, ExplicitExpression)
isOneOf (ExplicitPath e1 e2 c) 
    | not (null (conditions (typeFromExpression e1))) && 
        MakeCondition Nothing (Keyword "one-of") `elem` conditions (typeFromExpression e1) = Just (e1, e2)
    | otherwise = Nothing
isOneOf _ = Nothing

getCases :: (ExplicitExpression, Coercion) -> Doc a
getCases (ExplicitIfElse (ExplicitFunction op e rc, c) thn els coer, retCoer) 
    | op `elem` ["exists", "only exists"] = case isOneOf (fst (head e)) of
        Nothing -> "_" <+> "->" <+> printExpression (ExplicitIfElse (ExplicitFunction op e rc, c) thn els coer)
        Just ex -> pretty (typeName (typeFromExpression (fst ex))) <> pretty (typeName (typeFromExpression (snd ex))) <+> "x" <+> "->" 
                <+> printCoercion (snd thn) (printExpression (replace (fst (head e)) (fst thn) "x")) <> line <>
                getCases els 
getCases (e, c) = "_" <+> "->" <+> printCoercion c (printExpression e)

replace :: ExplicitExpression -> ExplicitExpression -> String -> ExplicitExpression
replace e1 (ExplicitPath pe1 pe2 c) x
    | e1 == ExplicitPath pe1 pe2 c = ExplicitVariable x c
    | otherwise = ExplicitPath (replace e1 pe1 x) pe2 c
replace e1 (ExplicitListUnaryOp op e2 c) x = ExplicitListUnaryOp op (replace e1 e2 x) c
replace e1 (ExplicitListOp op e2 cond c) x = ExplicitListOp op (replace e1 e2 x) (replace e1 cond x) c
replace e1 (ExplicitParens e2 c) x = ExplicitParens (replace e1 e2 x) c
replace e1 (ExplicitFunction op e2 c) x = ExplicitFunction op [(replace e1 (fst e) x, snd e) | e <- e2] c
replace e1 (ExplicitIfSimple cond e2 c) x = ExplicitIfSimple (replace e1 (fst cond) x, snd cond) (replace e1 (fst e2) x, snd e2) c
replace e1 (ExplicitIfElse cond thn els c) x = ExplicitIfElse (replace e1 (fst cond) x, snd cond) (replace e1 (fst thn) x, snd thn) (replace e1 (fst els) x, snd els) c
replace _ e _ = e