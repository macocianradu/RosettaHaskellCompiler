module Utils.Utils where 
    
import Data.Either
import Data.Char

-- |Convert a namespace to a filename
namespaceToName :: String -> String 
namespaceToName [] = ".rosetta"
namespaceToName ".*" = ".rosetta"
namespaceToName (c : cs)
    | c == '.' = '-' : namespaceToName cs
    | otherwise = c : namespaceToName cs

-- |Returns the directory of a file from a path
fileDirectory :: String -> String
fileDirectory s = take (length s - length (fileName s)) s

-- |Returns the name of a file from a path
fileName :: String -> String
fileName path = reverse $ fileName1 $ reverse path

-- |Auxiliary function for the name of a file from a path
fileName1 :: String -> String
fileName1 [] = []
fileName1 (c : cs)
    | c == '/' = []
    | otherwise = c : fileName1 cs

-- |Create a new haskell filename based on the namespace
haskellFileName :: String -> String
haskellFileName s = "resources/Generated/" ++ removePeriods s ++".hs"

-- |Function to remove all the periods from a name, and convert the name to CamelCase
removePeriods :: String -> String
removePeriods [] = []
removePeriods ['*'] = []
removePeriods (c:cs) = toUpper c : removePeriods1 cs

-- |Auxiliary function for converting names
removePeriods1 :: String -> String
removePeriods1 [] = []
removePeriods1 (c:cs) 
    | c == '.' = removePeriods cs
    | otherwise = c : removePeriods1 cs

-- |Extract the first elements from a list of tuples
fstlst :: [(a, b)] -> [a]
fstlst [] = []
fstlst ((a,_) : rst) = a : fstlst rst

-- |Extract the second elements from a list of tuples
sndlst :: [(a, b)] -> [b]
sndlst [] = []
sndlst ((_, b): rst) = b : sndlst rst

nestedLefts :: [[Either a b]] -> [[a]]
nestedLefts = map lefts

nestedRights :: [[Either a b]] -> [[b]]
nestedRights = map rights

-- |Get the objects from a pair with an either
pairLefts :: [(a, [Either b c])] -> [(a, [b])]
pairLefts [] = []
pairLefts ((a, b) : rst) = (a, lefts b) : pairLefts rst

-- |Get the objects from a pair with an either
pairRights :: [(a, [Either b c])] -> [(a, [c])]
pairRights [] = []
pairRights ((a, c) : rst) = (a, rights c) : pairRights rst