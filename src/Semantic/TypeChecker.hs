module Semantic.TypeChecker where

import Model.Type

data TypeCheckError =
   UndefinedType String
   | IfConditionNotBoolean
   | IfExpressionsDifferentTypes
   | UndefinedFunction String
   | ErrorInsideFunction
   | UndefinedVariable String
   | TypeMismatch String String
   deriving (Show)

checkAttributes :: [Type] -> [TypeAttribute] -> [Either TypeCheckError Type]
checkAttributes _ [] = []
checkAttributes definedTypes ((MakeTypeAttribute _ name _ _):as) = checkType definedTypes name : checkAttributes definedTypes as 

checkType :: [Type] -> Type -> Either TypeCheckError Type
checkType _ (MakeType "int" _ _ _) = Right $ BasicType "Integer"
checkType _ (MakeType "string" _ _ _) = Right $ BasicType "String"
checkType _ (MakeType "number" _ _ _) = Right $ BasicType "Double"
checkType _ (MakeType "boolean" _ _ _) = Right $ BasicType "Bool"
checkType _ (MakeType "time" _ _ _) = Right $ BasicType "Time"
checkType definedTypes name
    | name `elem` definedTypes = Right name
    | otherwise = Left $ UndefinedType (typeName name)
    
addDefinedTypes :: [Type] -> [Type] -> [Type]
addDefinedTypes l [] = l
addDefinedTypes l (BasicType _ : ts) = addDefinedTypes l ts
addDefinedTypes l (t:ts) = t : addDefinedTypes l ts