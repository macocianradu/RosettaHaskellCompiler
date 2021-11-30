module PrettyPrinter.RosettaObject where

import Model.RosettaObject
import PrettyPrinter.Enum
import PrettyPrinter.Function
import PrettyPrinter.Type
  
-- |Converts a supported Rosetta object into a haskell valid String
printRosettaObject :: RosettaObject -> String
printRosettaObject (EnumObject a) = printEnum a
printRosettaObject (TypeObject a) = printType a
printRosettaObject (FunctionObject a) = printFunction a