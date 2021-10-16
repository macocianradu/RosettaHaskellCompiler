module Model.Function where

import Model.Type (TypeAttribute)
  
data Function = 
    MakeFunction {
        functionName :: String,
        functionDescription :: Maybe String,
        inputParameters :: [TypeAttribute],
        outputParameter :: TypeAttribute,
        assignments :: [(Expression, Expression)]
    }
    deriving (Show)
    
data Condition =
    MakeCondition {
        conditionDescription :: Maybe String,
        conditionStatement :: Expression
--        conditionStatement :: String
    }
    | MakePostCondition {
        conditionDescription :: Maybe String,
        conditionStatement :: Expression
--        conditionStatement :: String
    }
    deriving (Show)
        
data Expression = --String deriving (Show)
    Variable String
    | Literal String
    | ExpressionList [Expression]
    | InnerType Expression Expression
    
    | Or Expression Expression
    | And Expression Expression
    | Not Expression
    
    | Exists Expression
    | IsAbsent Expression
    | Contains Expression Expression
    | Disjoint Expression Expression
    | Count Expression
    | OnlyExists Expression
    | OnlyElement Expression
    
    | Equals Expression Expression
    | Different Expression Expression
    | GreaterStrict Expression Expression
    | SmallerStrict Expression Expression
    | GreaterOrEqual Expression Expression
    | SmallerOrEqual Expression Expression
    
    | Sum Expression Expression
    | Subtract Expression Expression
    | Product Expression Expression
    | Division Expression Expression
    
    | IfSimple Expression Expression
    | IfElse Expression Expression Expression
    deriving (Show)