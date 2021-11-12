module Model.Function where

import Model.Type (TypeAttribute)
  
data Function = 
    MakeFunction {
        functionName :: String,
        functionDescription :: Maybe String,
        inputParameters :: [TypeAttribute],
        outputParameter :: TypeAttribute,
        assignment :: Expression
    }
    deriving (Show)
    
--data Condition =
--    MakeCondition {
--        conditionDescription :: Maybe String,
--        conditionStatement :: Expression
--    }
--    | MakePostCondition {
--        conditionDescription :: Maybe String,
--        conditionStatement :: Expression
--    }
--    deriving (Show)
     
data Expression = Variable String
    | Int String
    | Real String
    | Boolean String
    | Empty
    | Parens Expression
    | List [Expression]
    | Function String [Expression]
    | PrefixExp String Expression
    | PostfixExp String Expression
    | InfixExp String Expression Expression
    | IfSimple Expression Expression
    | IfElse Expression Expression Expression
    deriving (Eq, Show)