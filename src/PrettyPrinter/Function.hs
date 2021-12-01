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
printFunctionBody (MakeFunction name _ inp _ ex) = pretty name <+> printVariableNames inp <+> "=" <+> printExpression ex
printExpression :: Expression -> Doc a
printExpression (Variable s) = pretty s
printExpression (Int s) = pretty s
printExpression (Real s) = pretty s
printExpression (Boolean s) = pretty s
printExpression Empty = "empty"
printExpression (Parens ex) = "(" <> printExpression ex <> ")"
printExpression (List ex) = list (map printExpression ex)
printExpression (Function name ex) = pretty name <> tupled (map printExpression ex) 
printExpression (PrefixExp name ex) = pretty name <+> printExpression ex
printExpression (PostfixExp "exists" ex) = "isJust" <+> printExpression ex
printExpression (PostfixExp "is absent" ex) = "isNothing" <+> printExpression ex
printExpression (PostfixExp "single exists" ex) = "length" <+> printExpression ex <+> "==" <+> "1"
printExpression (PostfixExp "multiple exists" ex) = "length" <+> printExpression ex <+> ">" <+> "1" 
printExpression (PostfixExp "count" ex) = "length" <+> printExpression ex
printExpression (PostfixExp name ex) = pretty name <+> printExpression ex
-- Equality expressions
printExpression (InfixExp "=" ex1 ex2) = printExpression ex1 <+> "==" <+> printExpression ex2
printExpression (InfixExp "<>" ex1 ex2) = printExpression ex1 <+> "/=" <+> printExpression ex2
printExpression (InfixExp "any =" ex1 ex2) = printExpression ex2 <+> "`elem`" <+> printExpression ex1
printExpression (InfixExp "all <>" ex1 ex2) = printExpression ex2 <+> "`notElem`" <+> printExpression ex1
printExpression (InfixExp "and" ex1 ex2) = printExpression ex1 <+> "&&" <+> printExpression ex2
printExpression (InfixExp "or" ex1 ex2) = printExpression ex1 <+> "||" <+> printExpression ex2
printExpression (InfixExp name ex1 ex2) = printExpression ex1 <+> pretty name <+> printExpression ex2
printExpression (IfSimple cond ex) = "if" <+> printExpression cond <+> "then" <+> printExpression ex <+> "else" <+> "Nothing"
printExpression (IfElse cond ex1 ex2) = "if" <+> printExpression cond <+> "then" <+> printExpression ex1 <+> "else" <+> printExpression ex2

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