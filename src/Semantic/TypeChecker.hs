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

checkAttributes :: [Type] -> [TypeAttribute] -> [Either Type TypeCheckError]
checkAttributes _ [] = []
checkAttributes definedTypes ((MakeTypeAttribute _ name _ _):as) = checkType definedTypes name : checkAttributes definedTypes as 

checkType :: [Type] -> Type -> Either Type TypeCheckError
checkType _ (MakeType "int" _ _ _) = Left $ BasicType "Integer"
checkType _ (MakeType "string" _ _ _) = Left $ BasicType "String"
checkType _ (MakeType "number" _ _ _) = Left $ BasicType "Double"
checkType _ (MakeType "boolean" _ _ _) = Left $ BasicType "Bool"
checkType _ (MakeType "time" _ _ _) = Left $ BasicType "Time"
checkType definedTypes name
    | name `elem` definedTypes = Left name
    | otherwise = Right $ UndefinedType (typeName name)
    
addDefinedTypes :: [Type] -> [Type] -> [Type]
addDefinedTypes l [] = l
addDefinedTypes l (BasicType _ : ts) = addDefinedTypes l ts
addDefinedTypes l (t:ts) = t : addDefinedTypes l ts