module Model.RosettaObject where

import Model.Enum
import Model.Function
import Model.Type
  
-- |Any supported Rosetta object
data RosettaObject = 
    EnumObject EnumType
    | TypeObject Type
    | FunctionObject Function
    deriving Show

data CheckedRosettaObject =
    CheckedEnumObject EnumType
    | CheckedTypeObject Type
    | CheckedFunctionObject ExplicitFunction
    deriving Show