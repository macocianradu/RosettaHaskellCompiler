module Semantic.TypeChecker where

import Model.Type
import Data.Either
import Utils.Utils

-- |A datatype for the different types of type check errors
data TypeCheckError =
   UndefinedType String
   | IfConditionNotBoolean String
   | IfExpressionsDifferentTypes String String
   | UndefinedFunction String
   | ErrorInsideFunction String
   | UndefinedVariable String
   | TypeMismatch String String
   | CardinalityMismatch Cardinality Cardinality
   | MultipleDeclarations String
   deriving (Show)

-- |Checks whether a data type is valid
checkType :: [Type] -> Type -> Either [TypeCheckError] Type
checkType definedTypes (MakeType name super desc attr)
    | null (lefts checkedAttr) = case populateSuper definedTypes definedTypes super of
        Right superPopulated -> Right $ MakeType name superPopulated desc (rights checkedAttr)
        Left err -> Left [err]
    | otherwise = Left $ lefts checkedAttr  
    where checkedAttr = checkAttributes definedTypes attr
checkType _ (BasicType b) = Right (BasicType b)

populateSuper :: [Type] -> [Type] -> Type -> Either TypeCheckError Type
populateSuper _ _ (BasicType "Object") = Right (BasicType "Object")
populateSuper _ _ (BasicType _) = Left $ UndefinedType "Can't extend basic types"
populateSuper _ [] t = Left $ UndefinedType (typeName t)
populateSuper allTypes (currType : types) (MakeType t super d a) 
    | typeName currType == t = case populateSuper allTypes allTypes super of
        Right superChecked -> Right $ MakeType t superChecked (typeDescription currType) (typeAttributes currType)
        Left err -> Left err
    | otherwise = populateSuper allTypes types (MakeType t super d a)

-- |Checks whether all the types of the attributes of a data type are already defined
checkAttributes :: [Type] -> [TypeAttribute] -> [Either TypeCheckError TypeAttribute]
checkAttributes _ [] = []
checkAttributes definedTypes ((MakeTypeAttribute name typ crd desc):as)
    | isRight checked = Right (MakeTypeAttribute name (fromRightUnsafe checked) crd desc) : checkAttributes definedTypes as
    | otherwise = Left (fromLeftUnsafe checked) : checkAttributes definedTypes as
    where checked = checkAttributeType definedTypes typ 

-- |Checks whether a type is predefined or in the symbol table
checkAttributeType :: [Type] -> Type -> Either TypeCheckError Type
checkAttributeType _ (MakeType "int" _ _ _) = Right $ BasicType "Integer"
checkAttributeType _ (MakeType "string" _ _ _) = Right $ BasicType "String"
checkAttributeType _ (MakeType "number" _ _ _) = Right $ BasicType "Double"
checkAttributeType _ (MakeType "boolean" _ _ _) = Right $ BasicType "Bool"
checkAttributeType _ (MakeType "time" _ _ _) = Right $ BasicType "Time"
checkAttributeType definedTypes name
    | name `elem` definedTypes = Right name
    | otherwise = Left $ UndefinedType (typeName name)
    
-- |Add a list of defined types to the symbol table
addDefinedTypes :: [Type] -> [Type] -> [Type]
addDefinedTypes l [] = l
addDefinedTypes l (BasicType _ : ts) = addDefinedTypes l ts
addDefinedTypes l (t:ts)
    | typeName t `elem` map typeName l = error $ "Multiple declarations of " ++ show t
    | otherwise = t : addDefinedTypes l ts