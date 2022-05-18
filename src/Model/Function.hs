module Model.Function where

import Model.Type (TypeAttribute, Expression, ExplicitExpression)
  
data FunctionSignature = 
    MakeFunctionSignature {
        functionName :: String,
        functionDescription :: Maybe String,
        inputParameters :: [TypeAttribute],
        outputParameter :: TypeAttribute
    }
    deriving (Show)

-- |The representation of a Rosetta function type
data Function = 
    MakeFunction {
        signature :: FunctionSignature,
        aliases :: [(String, Expression)],
        assignment :: [(Expression, Expression)]
    }
    deriving (Show)

data ExplicitFunction = 
    MakeExplicitFunction {
        sign :: FunctionSignature,
        explicitAliases :: [(String, ExplicitExpression)],
        explicitAssignment :: [(ExplicitExpression, ExplicitExpression)] 
    }
    deriving Show