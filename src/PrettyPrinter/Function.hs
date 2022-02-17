{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Function where

import Prettyprinter
import Model.Function
import PrettyPrinter.General
import PrettyPrinter.Type
import Model.Type
  
-- show printStatementTree 

-- |Converts a Function into a haskell valid String
printFunction :: Function -> String
printFunction f = show $ vcat [printFunctionSignature f, printFunctionBody f, emptyDoc]

-- |Converts the body of a Function into a haskell valid Doc
printFunctionBody :: Function -> Doc a
printFunctionBody (MakeFunction name _ inp out ex) = pretty name <+> printVariableNames inp <+> "=" <+> printExpression inp (cardinality out) ex
printExpression :: [TypeAttribute] -> Cardinality -> Expression -> Doc a
printExpression inps c (Variable s) = printVariable varC c s
    where 
        varC = getVarCardinality inps s
printExpression inps c (Int s) = printVariable (Bounds (1, 1)) c s
printExpression inps c (Real s) = printVariable (Bounds (1, 1)) c s
printExpression inps c (Boolean s) = printVariable (Bounds (1, 1)) c s
printExpression inps c Empty = "empty"
printExpression inps c (Parens ex) = "(" <> printExpression inps c ex <> ")"
printExpression inps c (List ex) = list (map (printExpression inps c) ex)
printExpression inps c (Function name ex) = pretty name <> tupled (map (printExpression inps c) ex) 
printExpression inps c (PrefixExp name ex) = pretty name <+> printExpression inps c ex
printExpression inps c (PostfixExp "exists" ex) = "isJust" <+> printExpression inps c ex
printExpression inps c (PostfixExp "is absent" ex) = "isNothing" <+> printExpression inps c ex
printExpression inps c (PostfixExp "single exists" ex) = "length" <+> printExpression inps c ex <+> "==" <+> "1"
printExpression inps c (PostfixExp "multiple exists" ex) = "length" <+> printExpression inps c ex <+> ">" <+> "1" 
printExpression inps c (PostfixExp "count" ex) = "length" <+> printExpression inps c ex
printExpression inps c (PostfixExp name ex) = pretty name <+> printExpression inps c ex
-- Equality expressions
-- [a] a all = 
-- any <>
printExpression inps c (InfixExp "=" ex1 ex2) = printExpression inps c ex1 <+> "==" <+> printExpression inps c ex2
printExpression inps c (InfixExp "<>" ex1 ex2) = printExpression inps c ex1 <+> "/=" <+> printExpression inps c ex2
printExpression inps c (InfixExp "any =" ex1 ex2) = printExpression inps c ex2 <+> "`elem`" <+> printExpression inps c ex1
printExpression inps c (InfixExp "all <>" ex1 ex2) = printExpression inps c ex2 <+> "`notElem`" <+> printExpression inps c ex1
--printExpression (InfixExp "all =" ex1 ex2) = "all (Eq)" <+> printExpression ex2 <+> printExpression ex1
printExpression inps c (InfixExp "and" ex1 ex2) = printExpression inps c ex1 <+> "&&" <+> printExpression inps c ex2
printExpression inps c (InfixExp "or" ex1 ex2) = printExpression inps c ex1 <+> "||" <+> printExpression inps c ex2
printExpression inps c (InfixExp name ex1 ex2) = printExpression inps c ex1 <+> pretty name <+> printExpression inps c ex2
printExpression inps (Bounds (0, 1)) (IfSimple cond ex) = "if" <+> printExpression inps (Bounds (1, 1)) cond <+> "then" <+> printExpression inps (Bounds (0, 1)) ex <+> "else" <+> "Nothing"
printExpression inps c (IfSimple cond ex) = "if" <+> printExpression inps (Bounds (1, 1)) cond <+> "then" <+> printExpression inps c ex <+> "else" <+> "[]"
printExpression inps c (IfElse cond ex1 ex2) = "if" <+> printExpression inps (Bounds (1, 1)) cond <+> "then" <+> printExpression inps c ex1 <+> "else" <+> printExpression inps c ex2

-- |Converts a variable into a maybe or list depending on necessity
printVariable :: Cardinality -> Cardinality -> String -> Doc a
printVariable (Bounds (1, 1)) (Bounds (1, 1)) s = pretty s
printVariable (Bounds (1, 1)) (Bounds (0, 1)) s = "Just" <+> pretty s
printVariable (Bounds (0, 1)) (Bounds (0, 1)) s = pretty s
printVariable (Bounds (1, 1)) _ s = "[" <+> pretty s <+> "]"
printVariable _ _ s = pretty s

-- |Converts a list of type attributes to a Doc with a list of variable names
printVariableNames :: [TypeAttribute] -> Doc a
printVariableNames vars = foldl (<+>) emptyDoc (map (pretty . attributeName) vars)

-- |Converts a function into a haskell valid Doc representing the signature of the function
printFunctionSignature :: Function -> Doc a
printFunctionSignature (MakeFunction name description inputs output _) =
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