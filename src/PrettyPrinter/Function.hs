{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Function where

import Prettyprinter
import Model.Function
import PrettyPrinter.General
import PrettyPrinter.Type
  
-- show printStatementTree 

printFunction :: Function -> String
printFunction f = show $ vcat [printFunctionSignature f, printFunctionBody f]

printFunctionBody :: Function -> Doc a
printFunctionBody (MakeFunction name _ _ _ ex) = pretty name <+> "=" <+> printExpression ex
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
printExpression (PostfixExp name ex) = printExpression ex <+> pretty name
printExpression (InfixExp name ex1 ex2) = printExpression ex1 <+> pretty name <+> printExpression ex2
printExpression (IfSimple cond ex) = "if" <+> printExpression cond <+> "then" <+> printExpression ex
printExpression (IfElse cond ex1 ex2) = "if" <+> printExpression cond <+> "then" <+> printExpression ex1 <+> "else" <+> printExpression ex2

printFunctionSignature :: Function -> Doc a
printFunctionSignature (MakeFunction name description inputs output _) =
    printDescription description (pretty name <+> prettyPrintType (Prelude.map printCardinality (inputs ++ [output])))

prettyPrintType :: [Doc x] -> Doc x
prettyPrintType = align . sep . Prelude.zipWith (<+>) ("::" : repeat "->")