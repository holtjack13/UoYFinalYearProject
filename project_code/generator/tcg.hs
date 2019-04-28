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
    mainFunction <- generateMain
    parseFunction <- generateSourceLangParseFunction
    code <- generateTypeCheckingCode
    writeFile (specName ++ "TypeChecker.hs") (moduleImports ++ 
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

generateMain :: IO String
generateMain = do
    specName <- getSpecFileName
    let mainFunction = "main :: IO ()\n" ++
                       "main = do\n" ++
                        "   cmdLineArgs <- getArgs\n" ++ 
                        "   let srcFile = head cmdLineArgs\n" ++
                        "   srcParseTree <- parseSourceFile srcFile\n" ++
                        "   case srcParseTree of\n" ++
                        "       Ok tree -> check tree\n" ++
                        "       Bad err -> putStrLn err\n" ++
                        "   return ()\n"
                        "\n"
    return mainFunction

generateSourceLangParseFunction :: IO String
generateSourceLangParseFunction = do
    specName <- getSpecFileName
    let parseFunction = "parseSourceFile :: String -> IO (Err Specification)\n" ++
                        "parseSourceFile f = do\n" ++
                        "   sourceString <- readFile f\n" ++
                        "   return $ sourceLexAndParse sourceString\n" ++
                        "       where\n" ++
                        "           sourceLexAndParse = (pProgram . Par" ++
                                                        specName ++ ")\n" ++
                        "\n"
    return parseFunction


generateTypeCheckingCode :: IO String
generateTypeCheckingCode = do
    errSpec <- parseSpec "Test"
    case errSpec of 
      Ok spec -> return $ evalSpec spec
      Bad err -> return err


parseSpec :: String -> IO (Err Specification)
parseSpec specFile = do
    specString <- readFile specFile
    return $ specLexAndParse specString 
        where
            specLexAndParse = (pSpecification . ParTsl.myLexer)


evalSpec :: Specification -> String
evalSpec (Spec ms rs) = mconcat $ map (evalRule ms) rs


evalRule :: TypeMap -> TypeRule -> String
evalRule ms (RuleNoSC name as c) = (generateFunctionSignature ms c) 
                                    ++ (generateFunctionBody ms as)
evalRule ms (RuleSC name as sc c) = ""


generateFunctionSignature :: TypeMap -> Judgement -> String
generateFunctionSignature ms (Jmnt e t) = "check (" ++ e ++ ") " ++ 
                                           expand t ms ++ " = "


expand :: TSLType -> TypeMap -> String
expand t [] = ""
expand TStm ms = "()"
expand t (m@(TMap s t'):ms) = if t == t' then s else expand t ms


generateFunctionBody :: TypeMap -> [Judgement] -> String
generateFunctionBody ms [] = "True\n"
generateFunctionBody ms js = mconcat ([(genFB ms j') ++ "&& " | j' <- init js] 
                                            ++ [(genFB ms (last js)) ++ "\n"])


genFB :: TypeMap -> Judgement -> String
genFB ms (Jmnt e t) = "check " ++ e ++ " " ++ expand t ms ++ " "


