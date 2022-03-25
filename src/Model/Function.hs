module Model.Function where

import Model.Type (TypeAttribute, Expression)
  
-- |The representation of a Rosetta function type
data Function = 
    MakeFunction {
        functionName :: String,
        functionDescription :: Maybe String,
        inputParameters :: [TypeAttribute],
        outputParameter :: TypeAttribute,
        assignment :: Expression
    }
    deriving (Show)