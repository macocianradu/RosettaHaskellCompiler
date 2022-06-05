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
import System.Environment.Blank (getArgs)
import Model.Enum
import Data.Either
import Model.Header
import Parser.Header
import PrettyPrinter.Header
import Data.Tuple (fst, snd)
import Data.Void
import Utils.Utils
import Data.Text (Text)
import Parser.Expression (expressionParser)
-- :l resources/Generated/ContractDSL.hs resources/Generated/ImportsTypes.hs resources/Generated/ImportsEnums.hs
-- :set args resources/Rosetta/Contracts/contractDSL.rosetta

-- |Reads a rosetta string from the first input argument and writes a haskell output to the file given as a second argument
main :: IO ()
main = do
    args <- getArgs
    let mainFile = head args
    parseResult <- parseWithImport mainFile
    let checked = checkObjects parseResult
    let headers = fstlst checked
    let objects = nestedRights $ sndlst checked
    if null $ lefts $ concat $ sndlst checked
        then
            let input = pairRights checked in
                mapM_ generateFile input
        else error $ show $ lefts $ concat $ sndlst checked

{- |Recursively parse a file and all the imports into a list of headers and objects
The first argument is the default directory, second argument is the file name
-}
parseWithImport :: String -> IO [(([Type], [Symbol]), (Header, [RosettaObject]))]
parseWithImport file =
    do
        plain <- readFile file
        case parseFile plain of
            Left errorBundle -> error $ errorBundlePretty errorBundle ++ "on file" ++ file
            Right (MakeHeader name desc vers imp, objs) ->
                do
                    let files = map ((++) (fileDirectory file) . namespaceToName) imp
                    imports <- mapM parseWithImport files
                    let importedSymbolTable = fstlst (concat imports)
                    let importedTypes = concat $ fstlst importedSymbolTable
                    let importedFunctions = concat $ sndlst importedSymbolTable
                    case addNewTypes importedTypes objs of
                        Left errors -> error $ show errors
                        Right definedTypes -> case addNewFunctions (definedTypes, importedFunctions) objs of
                            Left errors -> error $ show errors
                            Right definedFunctions -> return $ ((definedTypes, definedFunctions), (MakeHeader name desc vers imp, objs)) : concat imports

-- |Parse a file into a list of RosettaObjects
parseFile :: String -> Either (ParseErrorBundle Text Void) (Header, [RosettaObject])
parseFile plainText = parse rosettaParser "" (Text.pack plainText)

-- |Converts a RosettaObject into a plain haskell string
-- printObject :: CheckedRosettaObject -> String
-- printObject (CheckedTypeObject t) = printType t
-- printObject (CheckedFunctionObject f) = printFunction f
-- printObject (CheckedEnumObject e) = printEnum e

-- |Checks all the objects from a list
checkObjects :: [(([Type], [Symbol]), (Header, [RosettaObject]))] -> [(Header, [Either [TypeCheckError] CheckedRosettaObject])]
checkObjects [] = []
checkObjects (((definedTypes, definedSymbols), (header, objs)) : rest) = (header, checked) : checkObjects rest
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
generateFile :: (Header, [CheckedRosettaObject]) -> IO ()
generateFile (header, objects) = writeFile (haskellFileName $ namespace header) (printHeader header ++ concatMap printRosettaObject objects)