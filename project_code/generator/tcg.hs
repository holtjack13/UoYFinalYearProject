module TypeCheckerGenerator where

import Data.Monoid

-- Type System Language Modules
import AbsTsl
import ParTsl

import ErrM

import System.Directory
import Data.List
import Data.Char

type TypeMap = [TypeMapping]

main :: IO ()
main = buildTypeCheckerFile


buildTypeCheckerFile :: IO ()
buildTypeCheckerFile = do
    specName <- getSpecFileName
    moduleImports <- generateModuleImports
    helperFunctions <- generateHelperFunctions
    mainFunction <- generateMain
    parseFunction <- generateSourceLangParseFunction
    code <- generateTypeCheckingCode
    writeFile (specName ++ "TypeChecker.hs") (moduleImports ++ 
                                              helperFunctions ++
                                              mainFunction ++
                                              parseFunction ++
                                              code)

getSpecFileName :: IO String
getSpecFileName = do
    workingDirectory <- getCurrentDirectory
    filesInDirectory <- listDirectory $ workingDirectory
    fileName <- return $ (prefix . head) $ filter (isInfixOf ".cf") filesInDirectory
    return $ (toUpper . head . transformName) fileName : (tail . transformName) fileName
        where
            prefix s = takeWhile (/='.') s
            stripInvalidChars s = [c | c <- s, not (c `elem` "_-+=£")] 
            transformName = (stripInvalidChars . upperAfterUnderscore) 


upperAfterUnderscore :: String -> String
upperAfterUnderscore [] = ""
upperAfterUnderscore ('_':cs) = '_' : 
                                upperAfterUnderscore (capitalFirstLetter cs: tail cs)
    where
        capitalFirstLetter s = (toUpper . head) s
upperAfterUnderscore (c:cs) = c : upperAfterUnderscore cs 


generateModuleImports :: IO String
generateModuleImports = do
    specName <- getSpecFileName
    let moduleImports = "module " ++ specName ++ "TypeChecker where \n" ++
                        "\n" ++
                        "import Abs" ++ specName ++ "\n" ++
                        "import Par" ++ specName ++ "\n" ++
                        "import ErrM" ++ "\n" ++
                        "import System.Environment\n" ++
                        "\n"
    return moduleImports

generateHelperFunctions :: IO String
generateHelperFunctions = do
    specName <- getSpecFileName
    let helperFunctions = "safeAnd :: Err Bool -> Err Bool -> Err Bool\n" ++
                          "safeAnd (Ok a) (Ok b) = Ok (a && b)\n" ++
                          "safeAnd (Bad s) (Ok b) = Bad s\n" ++
                          "safeAnd (Ok a) (Bad s) = Bad s\n" ++
                          "safeAnd (Bad s) (Bad t) = Bad (s ++ \"\\n\" ++ t)" ++
                          "\n\n"
    return helperFunctions


generateMain :: IO String
generateMain = do
    specName <- getSpecFileName
    let mainFunction = "parseAndCheckSrc :: FilePath -> IO (Err Bool)\n" ++
                       "parseAndCheckSrc srcFile = do\n" ++
                        "   srcParseTree <- parseSourceFile srcFile\n" ++
                        "   case srcParseTree of\n" ++
                        "       Ok tree -> return $ checkProg tree ()\n" ++
                        "       Bad err -> return $ Bad err \n" ++
                        "\n"
    return mainFunction

generateSourceLangParseFunction :: IO String
generateSourceLangParseFunction = do
    specName <- getSpecFileName
    let parseFunction = "parseSourceFile :: String -> IO (Err Program)\n" ++
                        "parseSourceFile f = do\n" ++
                        "   sourceString <- readFile f\n" ++
                        "   return $ sourceLexAndParse sourceString\n" ++
                        "       where\n" ++
                        "           sourceLexAndParse = (pProgram . Par" ++
                                    specName ++ ".myLexer)\n" ++
                        "\n"
    return parseFunction


generateTypeCheckingCode :: IO String
generateTypeCheckingCode = do
    errSpec <- parseSpec "TypeRules"
    case errSpec of 
      Ok spec -> return $ evalSpec spec
      Bad err -> return err


parseSpec :: String -> IO (Err Spec)
parseSpec specFile = do
    specString <- readFile specFile
    return $ specLexAndParse specString 
        where
            specLexAndParse = (pSpec . ParTsl.myLexer)


evalSpec :: Spec -> String
evalSpec (SpecExpr ms p es) = mconcat $ [comment] ++ 
                              [evalRule ms p] ++ ["\n"] ++
                              map (evalRule ms) es ++ 
                              [genCheckExpErr] ++ ["\n"]
                                  where
                                      comment = "-- Type Checking Functions\n"
evalSpec (SpecExprStm ms p es ss) = mconcat $ [comment] ++ 
                                    [evalRule ms p] ++ ["\n"] ++
                                    map (evalRule ms) es ++ 
                                    [genCheckExpErr] ++ ["\n"] ++
                                    map (evalRule ms) ss ++ 
                                    [genCheckStmErr] ++ ["\n"]
                                  where
                                      comment = "-- Type Checking Functions\n"


evalRule :: TypeMap -> TypeRule -> String
evalRule ms (RuleNoSC name as c) = (generateFunctionSignature ms c) 
                                    ++ (generateFunctionBody ms as)
evalRule ms (RuleSC name as sc c) = ""


generateFunctionSignature :: TypeMap -> Judgement -> String
generateFunctionSignature ms (Jmnt e TProg) = "checkProg (" ++ e ++ ") " ++ 
                                           expand TStm ms ++ " = "
generateFunctionSignature ms (Jmnt e TStm) = "checkStm (" ++ e ++ ") " ++ 
                                           expand TStm ms ++ " = "
generateFunctionSignature ms (Jmnt e t) = "checkExp (" ++ e ++ ") " ++ 
                                           expand t ms ++ " = "


expand :: TSLType -> TypeMap -> String
expand t [] = ""
expand TStm ms = "()"
expand t (m@(TMap s t'):ms) = if t == t' then s else expand t ms


generateFunctionBody :: TypeMap -> [Judgement] -> String
generateFunctionBody ms [] = "Ok True\n"
generateFunctionBody ms js = mconcat ([(genCheckTerm ms j') ++ "`safeAnd` " | j' <- init js] 
                                            ++ [(genCheckTerm ms (last js)) ++ "\n"])


genCheckTerm :: TypeMap -> Judgement -> String
genCheckTerm ms (Jmnt e TStm) = "checkStm " ++ e ++ " " ++ expand TStm ms ++ " "
genCheckTerm ms (Jmnt e t) = "checkExp " ++ e ++ " " ++ expand t ms ++ " "

genCheckExpErr :: String
genCheckExpErr = "checkExp _ _ = Bad \"Error: Type error found in expression\"\n"

genCheckStmErr :: String
genCheckStmErr = "checkStm _ _ = Bad \"Error: Type error found in statement\"\n"

