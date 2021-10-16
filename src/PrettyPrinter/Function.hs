{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Function where

import Prettyprinter
import Model.Function
import PrettyPrinter.General
import PrettyPrinter.Type
  
-- show printStatementTree 

printFunctionSignature :: Function -> Doc a
printFunctionSignature (MakeFunction name description inputs output _) =
    printDescription description (pretty name <+> prettyPrintType (Prelude.map printCardinality (inputs ++ [output])))

prettyPrintType :: [Doc x] -> Doc x
prettyPrintType = align . sep . Prelude.zipWith (<+>) ("::" : repeat "->")