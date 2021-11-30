module Model.RosettaObject where

import Model.Enum
import Model.Function
import Model.Type
  
-- |Any supported Rosetta object
data RosettaObject = 
    EnumObject EnumType
    | TypeObject Type
    | FunctionObject Function