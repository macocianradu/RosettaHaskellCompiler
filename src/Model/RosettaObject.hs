module Model.RosettaObject where

import Model.Enum
import Model.Function
import Model.Type
  
data RosettaObject = 
    EnumObject EnumType
    | TypeObject Type
    | FunctionObject Function