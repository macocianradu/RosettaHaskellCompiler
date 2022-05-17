{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.Function where

import Prettyprinter
import PrettyPrinter.Expression
import Model.Function
import PrettyPrinter.General
import PrettyPrinter.Type
import Model.Type
import Utils.Utils (capitalize, uncapitalize)

{-
Consider all assignments as trees
Root must always bee the same type (output type of the functions)
Merge at every level, the nodes that have the same type
    1   1   1               1                   1                   1
    |   |   |             / | \               /   \               /   \
    2   2   4    --->   2   2   4    --->   2       4    --->   2       4
    |   |   |           |   |   |           | \     |           |       |
    3   3   5    --->   3   3   5    --->   3   3   5    --->   3       5
    |   |   |           |   |   |           |   |   |           | \     |
    EX1 EX2 EX3         EX1 EX2 EX3         EX1 EX2 EX3         EX1 EX2 EX3
-}

data AssignmentTree = AssignmentNode {
    var :: String,
    typ :: Type,
    children :: [AssignmentTree]
    }
    | AssignmentLeaf ExplicitExpression

instance Show AssignmentTree where
    show (AssignmentNode var typ child) = "Node: " ++ show (typeName typ) ++  show child
    show (AssignmentLeaf exp) = "Leaf: " ++ show exp

-- |Converts a Function into a haskell valid String
printFunction :: ExplicitFunction -> String
printFunction f = show $ vcat [printFunctionSignature (sign f), printFunctionBody f, emptyDoc]

-- |Converts the body of a Function into a haskell valid Doc
printFunctionBody :: ExplicitFunction -> Doc a
printFunctionBody (MakeExplicitFunction (MakeFunctionSignature name _ inp out) ex) =
    pretty name <+> printVariableNames inp <+> "=" <+> 
    printAssignmentTree (head $ mergeAssignmentTrees [convertToAssignmentTree (fst exp) (AssignmentLeaf (snd exp)) | exp <- ex])
    --error $ show $ mergeAssignmentTrees [convertToAssignmentTree (fst exp) (AssignmentLeaf (snd exp)) | exp <- ex]


-- |Converts a function into a haskell valid Doc representing the signature of the function
printFunctionSignature :: FunctionSignature -> Doc a
printFunctionSignature (MakeFunctionSignature name description inputs output) =
    printDescription description (pretty name <+> prettyPrintType (Prelude.map printCardinality (inputs ++ [output])))

-- |Zips the signature with the needed characters ('::', '->')
prettyPrintType :: [Doc x] -> Doc x
prettyPrintType = align . sep . Prelude.zipWith (<+>) ("::" : repeat "->")

printAssignmentTree :: AssignmentTree -> Doc a
printAssignmentTree (AssignmentLeaf exp) = printExpression exp
printAssignmentTree (AssignmentNode var typ c)
    | length c == 1 = case head c of
        AssignmentLeaf e -> printAssignmentTree (head c) 
        AssignmentNode v t _ -> printConstructor typ <> pretty (capitalize v) <+> "(" <> printAssignmentTree (head c) <> ")" 
    | otherwise = case typ of
        MakeType t _ _ _ _ -> "Make" <> pretty t <+> "{" <> hsep (punctuate "," [pretty (uncapitalize t) <> getVarName child <+> "=" <+> printAssignmentTree child | child <- c]) <> "}"
        BasicType _ -> sep ["(" <> printAssignmentTree child <> ")" | child <- c]

getVarName :: AssignmentTree -> Doc a
getVarName (AssignmentLeaf _) = emptyDoc
getVarName (AssignmentNode var _ _) = pretty (capitalize var)

mergeAssignmentTrees :: [AssignmentTree] -> [AssignmentTree]
mergeAssignmentTrees [] = []
mergeAssignmentTrees [a1] = [a1]
mergeAssignmentTrees (a1: a2 : as)
    | length merge == 1 = mergeAssignmentTrees [AssignmentNode {var = var a1, typ = typ a1, children = mergeAssignmentTrees (children (head merge))}] ++ as
    | otherwise = mergeAssignmentTrees (a1 : as) ++ mergeAssignmentTrees (a2 : as)
    where
        merge = mergeAssignmentTree a1 a2

mergeAssignmentTree :: AssignmentTree -> AssignmentTree -> [AssignmentTree]
mergeAssignmentTree (AssignmentNode var1 typ1 c1) (AssignmentNode var2 typ2 c2)
    | typ1 == typ2 && var1 == var2 = [AssignmentNode var1 typ1 (c1 ++ c2)]
    | otherwise = [AssignmentNode var1 typ1 c1, AssignmentNode var2 typ2 c2]
mergeAssignmentTree t1 t2 = [t1, t2]

-- |Convert an assignment expression into an assignment tree
convertToAssignmentTree :: ExplicitExpression -> AssignmentTree -> AssignmentTree
convertToAssignmentTree (ExplicitVariable a c) t = AssignmentNode {var = a, typ = coercionType $ typeCoercion c, children = [t]}
convertToAssignmentTree (ExplicitPath ex1 (ExplicitVariable a c) _) t =
    convertToAssignmentTree ex1 (AssignmentNode {var = a, typ = coercionType $ typeCoercion c, children = [t]})
convertToAssignmentTree e _ = error $ "Unsupported expression in path expression " ++ show e


-- |Prints the type name if it is new type, or nothing for basic types
printConstructor :: Type -> Doc a
printConstructor (MakeType a _ _ _ _) = pretty a
printConstructor (BasicType a) = emptyDoc