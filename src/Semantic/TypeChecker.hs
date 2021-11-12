module Semantic.TypeChecker where

import Model.Type
import Data.Either

data TypeCheckError =
   UndefinedType String
   | IfConditionNotBoolean
   | IfExpressionsDifferentTypes
   | UndefinedFunction String
   | ErrorInsideFunction String
   | UndefinedVariable String
   | TypeMismatch String String
   deriving (Show)

checkType :: [Type] -> Type -> Either [TypeCheckError] Type
checkType definedTypes (MakeType name super desc attr)
    | null (lefts checkedAttr) = Right $ MakeType name super desc (rights checkedAttr)
    | otherwise = Left $ lefts checkedAttr  
    where checkedAttr = checkAttributes definedTypes attr 
checkType _ (BasicType b) = Right (BasicType b)

checkAttributes :: [Type] -> [TypeAttribute] -> [Either TypeCheckError TypeAttribute]
checkAttributes _ [] = []
checkAttributes definedTypes ((MakeTypeAttribute name typ crd desc):as)
    | isRight checked = Right (MakeTypeAttribute name (fromRightUnsafe checked) crd desc) : checkAttributes definedTypes as
    | otherwise = Left (fromLeftUnsafe checked) : checkAttributes definedTypes as
    where checked = checkAttributeType definedTypes typ 

checkAttributeType :: [Type] -> Type -> Either TypeCheckError Type
checkAttributeType _ (MakeType "int" _ _ _) = Right $ BasicType "Integer"
checkAttributeType _ (MakeType "string" _ _ _) = Right $ BasicType "String"
checkAttributeType _ (MakeType "number" _ _ _) = Right $ BasicType "Double"
checkAttributeType _ (MakeType "boolean" _ _ _) = Right $ BasicType "Bool"
checkAttributeType _ (MakeType "time" _ _ _) = Right $ BasicType "Time"
checkAttributeType definedTypes name
    | name `elem` definedTypes = Right name
    | otherwise = Left $ UndefinedType (typeName name)
    
addDefinedTypes :: [Type] -> [Type] -> [Type]
addDefinedTypes l [] = l
addDefinedTypes l (BasicType _ : ts) = addDefinedTypes l ts
addDefinedTypes l (t:ts) = t : addDefinedTypes l ts

fromRightUnsafe :: (Show a) => Either a b -> b
fromRightUnsafe x = case x of
    Left a -> error ("Value is Left" ++ show a)
    Right b -> b
    
fromLeftUnsafe :: Either a b -> a
fromLeftUnsafe x = case x of
    Left a -> a
    Right _ -> error "Value is Right"