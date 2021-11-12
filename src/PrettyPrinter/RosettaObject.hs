module PrettyPrinter.RosettaObject where

import Model.RosettaObject
import PrettyPrinter.Enum
import PrettyPrinter.Function
import PrettyPrinter.Type
  
printRosettaObject :: RosettaObject -> String
printRosettaObject (EnumObject a) = printEnum a
printRosettaObject (TypeObject a) = printType a
printRosettaObject (FunctionObject a) = printFunction a