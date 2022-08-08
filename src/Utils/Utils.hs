module Utils.Utils where 
    
import Data.Either
import Data.Char
import Data.List (stripPrefix)


-- |Capitalize a string
capitalize :: String -> String
capitalize s = toUpper (head s) : tail s

-- |Uncapitalize a string
uncapitalize :: String -> String
uncapitalize s = toLower (head s) : tail s

-- |Convert a namespace to a filename
namespaceToName :: String -> String 
namespaceToName [] = ".rosetta"
namespaceToName ".*" = ".*"
namespaceToName (c : cs)
    | c == '.' = '-' : namespaceToName cs
    | otherwise = c : namespaceToName cs

-- |Create a new haskell filename based on the namespace
haskellFileName :: String -> String
haskellFileName s = "resources/Generated/" ++ removeChar s '-' ++".hs"

-- |Function to remove all the periods from a name, and convert the name to CamelCase
removeChar :: String -> Char -> String
removeChar [] _ = []
removeChar ['*'] _ = []
removeChar (c:cs) ch = toUpper c : removeChar1 cs ch

-- |Auxiliary function for converting names
removeChar1 :: String -> Char -> String
removeChar1 [] ch = []
removeChar1 (c:cs) ch 
    | c == ch = removeChar cs ch
    | otherwise = c : removeChar1 cs ch

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


-- |Check a list for duplicate values. Returns a list with all the values which have duplicates
checkDuplicates :: Eq a => [a] -> [a]
checkDuplicates [] = []
checkDuplicates (a : as)
    | a `elem` as = a : checkDuplicates as
    | otherwise = checkDuplicates as

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x


-- |If the second list contains the first list, it replaces that occurances with the third list
replacePrefix :: Eq a => [a] -> [a] -> [a] -> [a]
replacePrefix a b c = case stripPrefix a b of
    Nothing -> b
    Just bs -> c ++ bs

-- |Get the namespace name from the import
getNamespace :: String -> String
getNamespace [] = []
getNamespace ".*" = []
getNamespace (s : ss) = s : getNamespace ss