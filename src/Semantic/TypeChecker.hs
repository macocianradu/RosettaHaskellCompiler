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
   | TypeNameReserved String
   | UnsupportedExpressionInPathExpression String
   | ListOperationNotOnList String
   | PathExpressionOnList String
   deriving (Show)

-- |Checks whether a data type is valid
checkType :: [Type] -> Type -> Either [TypeCheckError] Type
checkType definedTypes (MakeType name super desc attr cond)
    | null (lefts checkedAttr) = case populateSuper definedTypes definedTypes super of
        Right superPopulated -> Right $ MakeType name superPopulated desc (rights checkedAttr) cond
        Left err -> Left [err]
    | otherwise = Left $ lefts checkedAttr  
    where checkedAttr = checkAttributes definedTypes attr
checkType _ (BasicType b) = Right (BasicType b)

populateSuper :: [Type] -> [Type] -> Type -> Either TypeCheckError Type
populateSuper _ _ (BasicType "Object") = Right (BasicType "Object")
populateSuper _ _ (BasicType _) = Left $ UndefinedType "Can't extend basic types"
populateSuper _ [] t = Left $ UndefinedType (typeName t)
populateSuper allTypes (currType : types) (MakeType t super d a c) 
    | typeName currType == t = case populateSuper allTypes allTypes super of
        Right superChecked -> Right $ MakeType t superChecked (typeDescription currType) (typeAttributes currType) c
        Left err -> Left err
    | otherwise = populateSuper allTypes types (MakeType t super d a c)

-- |Checks whether all the types of the attributes of a data type are already defined
checkAttributes :: [Type] -> [TypeAttribute] -> [Either TypeCheckError TypeAttribute]
checkAttributes _ [] = []
checkAttributes definedTypes ((MakeTypeAttribute name typ crd desc):as) =
    case checkAttributeType definedTypes typ of
        Left err -> Left err : checkAttributes definedTypes as
        Right checked -> Right (MakeTypeAttribute name checked crd desc) : checkAttributes definedTypes as 

-- |Checks whether a type is predefined or in the symbol table
checkAttributeType :: [Type] -> Type -> Either TypeCheckError Type
checkAttributeType [] t = Left $ UndefinedType $ typeName t
checkAttributeType _ (MakeType "int" _ _ _ _) = Right $ BasicType "Integer"
checkAttributeType _ (MakeType "string" _ _ _ _) = Right $ BasicType "String"
checkAttributeType _ (MakeType "number" _ _ _ _) = Right $ BasicType "Double"
checkAttributeType _ (MakeType "boolean" _ _ _ _) = Right $ BasicType "Bool"
checkAttributeType _ (MakeType "time" _ _ _ _) = Right $ BasicType "Time"
checkAttributeType (defined : ts) t
    | defined == t = Right defined
    | otherwise = checkAttributeType ts t

-- |Add a list of defined types to the symbol table
addDefinedTypes :: [Type] -> [Type] -> Either [TypeCheckError] [Type]
addDefinedTypes l [] = Right l
addDefinedTypes l (BasicType t : ts) =  Left [TypeNameReserved t] 
addDefinedTypes l (t:ts) =
    case addDefinedTypes l ts of
        Left error -> Left error
        Right types -> if typeName t `elem` map typeName l
            then Left [MultipleDeclarations $ show t]
            else Right $ t : types