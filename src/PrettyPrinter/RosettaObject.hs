module PrettyPrinter.RosettaObject where

import Model.RosettaObject
import PrettyPrinter.Enum
import PrettyPrinter.Function
import PrettyPrinter.Type
  
-- |Converts a supported Rosetta object into a haskell valid String
printRosettaObject :: CheckedRosettaObject -> String
printRosettaObject (CheckedEnumObject a) = printEnum a
printRosettaObject (CheckedTypeObject a) = printType a
printRosettaObject (CheckedFunctionObject a) = printFunction a