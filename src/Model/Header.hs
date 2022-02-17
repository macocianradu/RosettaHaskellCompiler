module Model.Header where

-- |Representation of the information stored in the file header
data Header = MakeHeader {
    namespace :: String,
    headerDescription :: Maybe String, 
    version :: String,
    imports :: [String]
} deriving (Show, Eq)