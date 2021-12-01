module Semantic.TypeChecker where

import Model.Type
import Data.Either

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
   deriving (Show)

-- |Checks whether a data type is valid
checkType :: [Type] -> Type -> Either [TypeCheckError] Type
checkType definedTypes (MakeType name super desc attr)
    | null (lefts checkedAttr) = case checkSuper definedTypes super of
        Right superChecked -> Right $ MakeType name superChecked desc (rights checkedAttr)
        Left err -> Left [err]
    | otherwise = Left $ lefts checkedAttr  
    where checkedAttr = checkAttributes definedTypes attr
checkType _ (BasicType b) = Right (BasicType b)


checkSuper :: [Type] -> Maybe Type -> Either TypeCheckError (Maybe Type)
checkSuper _ Nothing = Right Nothing
checkSuper definedTypes (Just super) = 
    case checkAttributeType definedTypes super of
        Right sup -> Right (Just sup)
        Left err -> Left err

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
addDefinedTypes l (t:ts) = t : addDefinedTypes l ts

-- |Auxiliary function to get the right value from an either that stops with an error if the value is left
-- used when it is certain that the value will be right
fromRightUnsafe :: (Show a) => Either a b -> b
fromRightUnsafe x = case x of
    Left a -> error ("Value is Left" ++ show a)
    Right b -> b
    
-- |Auxiliary function to get the left value from an either that stops with an error if the value is right
-- used when it is certain that the value will be left
fromLeftUnsafe :: Either a b -> a
fromLeftUnsafe x = case x of
    Left a -> a
    Right _ -> error "Value is Right"