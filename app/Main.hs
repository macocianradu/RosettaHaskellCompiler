{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser.Enum
import Parser.Type
import Parser.Function
import Parser.General
import Model.RosettaObject
import qualified Data.Text as Text
import Text.Megaparsec
import PrettyPrinter.Enum
import PrettyPrinter.Type
import PrettyPrinter.Function
import PrettyPrinter.RosettaObject
import Semantic.TypeChecker
import Semantic.ExpressionChecker
import Semantic.FunctionChecker
import Model.Type
import Model.Function
import System.Environment.Blank (getArgs)
import Model.Enum
import Data.Either
import Model.Header
import Parser.Header
import PrettyPrinter.Header
import Data.Tuple (fst, snd)
import Data.Void
import System.FilePath
import Utils.Utils
import System.Directory
import Data.List
import Data.Text (Text)
import Parser.Expression (expressionParser)
-- :l resources/Generated/ContractDSL.hs resources/Generated/ImportsType.hs resources/Generated/ImportsEnum.hs
-- :set args resources/Rosetta/Contracts/
-- :set args resources/CDM/

-- |Reads a rosetta string from the first input argument and writes a haskell output to the file given as a second argument
main :: IO ()
main = do
    args <- getArgs
    let mainFolder = head args
    files <- getDirectoryContents mainFolder
    parseResult <- parseFolder [mainFolder ++ f | f <- files, f `notElem` [".", ".."]]
    let completeFiles = [createSymbolTable f parseResult | f <- parseResult]
    let checked = checkObjects completeFiles
    let headers = fstlst checked
    let objects = nestedRights $ sndlst checked
    --_ <- error $ show [(map typeName t, map symbolName s) | (path, t, s, (head, obj)) <- completeFiles, path == "resources/Rosetta/Contracts/contractDSL.rosetta"]
    if null $ lefts $ concat $ sndlst checked
        then
            let input = pairRights checked in
                mapM_ generateFile input
        else error $ show $ lefts $ concat $ sndlst checked

{- | Parse the contents of a folder, skip any files without the Rosetta extension
Return a list of Header, Rosettaobject, filename pairs  
-}
parseFolder :: [FilePath] -> IO [(FilePath, Header, [RosettaObject])]
parseFolder [] = return []
parseFolder (file : files) = do
    plain <- readFile file
    rest <- parseFolder files
    case parseFile plain of
        Left errorBundle -> error $ errorBundlePretty errorBundle ++ " on file " ++ file
        Right (header, objs) ->
            return ((file, header, objs) : rest)

-- | Create the symbol table for each file from the imported types enums and functions
createSymbolTable :: (FilePath, Header, [RosettaObject]) ->  [(FilePath, Header, [RosettaObject])] -> (FilePath, [Type], [Symbol], (Header, [RosettaObject]))
createSymbolTable (file, MakeHeader name desc ver imp, objs) imps =
        (file, concat (fstlst imports), concat (sndlst imports), (MakeHeader name desc ver actualImport, objs))
        where
            imports = [(getTypes imObjs, getFunctions imObjs) | (_, MakeHeader imName _ _ _, imObjs) <- imps, imName `elem` map getNamespace imp || imName == name]
            actualImport = 
                [removeChar (takeFileName (dropExtension p)) '-' | (p, MakeHeader imName _ _ _, imObjs) <- imps, imName `elem` map getNamespace imp 
                || (imName == name && p /= file && not ("enum.rosetta" `isSuffixOf` file) && not ("type.rosetta" `isSuffixOf` file && "func.rosetta" `isSuffixOf` p))]

-- |Get the defined data types from a list of rosetta objects
getTypes :: [RosettaObject] -> [Type]
getTypes [] = []
getTypes (TypeObject o : os) = o{typeAttributes = map convertAttributeType (typeAttributes o)} : getTypes os
getTypes (EnumObject o : os) = convertEnumToType o : getTypes os
getTypes (FunctionObject o : os) = getTypes os

convertAttributeType :: TypeAttribute -> TypeAttribute
convertAttributeType (MakeTypeAttribute n (MakeType "int" _ _ _ _) c d) = MakeTypeAttribute n (BasicType "Integer") c d
convertAttributeType (MakeTypeAttribute n (MakeType "string" _ _ _ _) c d) = MakeTypeAttribute n (BasicType "String") c d
convertAttributeType (MakeTypeAttribute n (MakeType "number" _ _ _ _) c d) = MakeTypeAttribute n (BasicType "Double") c d
convertAttributeType (MakeTypeAttribute n (MakeType "boolean" _ _ _ _) c d) = MakeTypeAttribute n (BasicType "Bool") c d
convertAttributeType (MakeTypeAttribute n (MakeType "time" _ _ _ _) c d) = MakeTypeAttribute n (BasicType "Time") c d
convertAttributeType t = t

getFunctions :: [RosettaObject] -> [Symbol]
getFunctions [] = []
getFunctions (FunctionObject o : os) = functionToSymbol o : getFunctions os
getFunctions (o : os) = getFunctions os

functionToSymbol :: Function -> Symbol
functionToSymbol (MakeFunction (MakeFunctionSignature name _ inp out) _ _) = Func name [(attributeType (convertAttributeType i), Model.Type.cardinality i)| i <- inp] (attributeType out, Model.Type.cardinality out)

{- |Recursively parse a file and all the imports into a list of headers and objects
The first argument is a list of files already parsed (to handle cyclic imports) and the second is the name of the file to be parsed
-}
parseWithImport :: [String] -> String -> IO [(([Type], [Symbol]), (Header, [RosettaObject]))]
parseWithImport files file
    | file `elem` files = error $ "cyclic dependency on " ++ file
    | otherwise =
    do
        plain <- readFile file
        case parseFile plain of
            Left errorBundle -> error $ errorBundlePretty errorBundle ++ "on file" ++ file
            Right (MakeHeader name desc vers imp, objs) ->
                do
                    let modules = map namespaceToName imp
                    importFiles <- getFiles modules
                    imports <- mapM (parseWithImport (file : files)) importFiles
                    let importedSymbolTable = fstlst (concat imports)
                    let importedTypes = concat $ fstlst importedSymbolTable
                    let importedFunctions = concat $ sndlst importedSymbolTable
                    case addNewTypes importedTypes objs of
                        Left errors -> error $ show errors ++ "\n on file " ++ file
                        Right definedTypes -> case addNewFunctions (definedTypes, importedFunctions) objs of
                            Left errors -> error $ show errors ++ "\n on file " ++ file
                            Right definedFunctions -> return $ ((definedTypes, definedFunctions), (MakeHeader name desc vers imp, objs)) : concat imports


getFiles :: [String] -> IO [String]
getFiles [] = return []
getFiles (n : ns)
    | ".*" `isSuffixOf` n = do
        files <- getDirectoryContents $ takeDirectory n
        rest <- getFiles ns
        return $ [f | f <- files, dropExtension n `isPrefixOf` f] ++ rest
    | otherwise = do
        files <- getFiles ns
        return (n : files)

-- |Parse a file into a list of RosettaObjects
parseFile :: String -> Either (ParseErrorBundle Text Void) (Header, [RosettaObject])
parseFile plainText = parse rosettaParser "" (Text.pack plainText)

-- |Checks all the objects from a list
checkObjects :: [(FilePath, [Type], [Symbol], (Header, [RosettaObject]))] -> [((FilePath, Header), [Either [TypeCheckError] CheckedRosettaObject])]
checkObjects [] = []
checkObjects ((file, definedTypes, definedSymbols, (header, objs)) : rest) = ((file, header), checked) : checkObjects rest
    where
        checked = map (checkObject (definedTypes, definedSymbols)) objs

-- |Checks the RosettaObject for type errors
checkObject :: ([Type], [Symbol]) -> RosettaObject -> Either [TypeCheckError] CheckedRosettaObject
-- |Checks the type and attributes of a type
checkObject (definedTypes, _) (TypeObject t) =
    case checkType definedTypes t of
        Left errors -> Left errors
        Right typ -> Right $ CheckedTypeObject typ
-- |If an enum parses, it cannot throw an error
checkObject _ (EnumObject e) = Right $ CheckedEnumObject e
-- |Checks the function inputs, output and assignment
checkObject (definedTypes, definedFunctions) (FunctionObject fun) =
    case checkFunction (definedTypes, defaultMap ++ definedFunctions) fun of
        Left errors -> Left errors
        Right func -> Right $ CheckedFunctionObject func

-- |Adds new defined functions into the symbol table
addNewFunctions :: ([Type], [Symbol]) -> [RosettaObject] -> Either [TypeCheckError] [Symbol]
addNewFunctions (_, s) [] = Right []
addNewFunctions (t, s) ((FunctionObject f):os) =
    case addNewFunctions (t, s) os of
        Left errors -> Left errors
        Right symbs -> addFunction (t, symbs) f
addNewFunctions (t, s) (_:os) = addNewFunctions (t, s) os

-- |Adds new defined types into the symbol table
addNewTypes :: [Type] -> [RosettaObject] -> Either [TypeCheckError] [Type]
addNewTypes l [] = Right l
addNewTypes defined (TypeObject o: os) =
    case addNewTypes defined os of
        Left errors -> Left errors
        Right types -> addDefinedTypes types [o]
addNewTypes defined (EnumObject e: os) = addNewTypes defined (TypeObject (convertEnumToType e) : os)
addNewTypes defined (_ :os) = addNewTypes defined os

-- |Parses any supported Rosetta types into a list of RosettaObject
rosettaParser :: Parser (Header, [RosettaObject])
rosettaParser = do
    header <- headerParser
    objects <- many (try parseEnum <|> try parseType <|> try parseFunction) <* eof
    return (header, objects)

-- |Reads an enum into a RosettaObject
parseEnum :: Parser RosettaObject
parseEnum = do
    EnumObject <$> enumParser

-- |Parse a type into a RosettaObject
parseType :: Parser RosettaObject
parseType = do
    TypeObject <$> typeParser

-- |Parse a function into a RosettaObject
parseFunction :: Parser RosettaObject
parseFunction = do
    FunctionObject <$> functionParser

-- |Generate a new haskell file based on the rosetta objects and header
generateFile :: ((FilePath, Header), [CheckedRosettaObject]) -> IO ()
generateFile ((path, header), objects) =
    writeFile (haskellFileName $ dropExtension $ takeFileName path)
    (printHeader (dropExtension $ takeFileName path) header ++ concatMap printRosettaObject objects)