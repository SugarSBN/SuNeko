module Main where

import Lexer
import Parser
import Frontend.Syntax
import Backend.Environment
import Backend.Interpreter

test :: IO ()
test = do 
    env <- setupEnvironment
    prettyPrint env
    ans <- analyzeExpr (ExprAdd (ExprInt 4) (ExprInt 5)) env succ fail
    print ans
    ans <- analyzeExpr (ExprVar "True") env succ fail
    print ans
    ans <- analyzeExpr (ExprIf (ExprVar "True") (ExprInt 3) (ExprInt 4)) env succ fail
    print ans
    ans <- analyzeExpr (ExprIf (ExprVar "False") (ExprInt 3) (ExprInt 4)) env succ fail
    print ans
    ans <- analyzeExpr (ExprSet "True" (ExprAdd (ExprVar "True") (ExprInt 3))) env succ fail
    print ans 
    prettyPrint env 
    ans <- analyzeExpr (ExprBegin [ExprSet "True" (ExprInt 99), ExprVar "True"]) env succ fail
    print ans
    ans <- analyzeDefine (ExprDefine "x" (ExprAdd (ExprInt 4) (ExprInt 5))) env succ fail
    print ans 
    prettyPrint env 
    ans <- analyzeDefine (FuncDefine "+" ["x", "y"] (ExprAdd (ExprVar "x") (ExprVar "y"))) env succ fail
    print ans
    prettyPrint env
    where
        succ :: Succeed
        succ v fail = return v
        fail :: Fail
        fail = error "Fail"

main :: IO ()
main = do
    putStrLn "---------------------------------------"
    putStrLn "|    _____       _   __     __        |"
    putStrLn "|   / ___/__  __/ | / /__  / /______  |"
    putStrLn "|   \\__ \\/ / / /  |/ / _ \\/ //_/ __ \\ |"
    putStrLn "|  ___/ / /_/ / /|  /  __/ ,< / /_/ / |"
    putStrLn "| /____/\\__,_/_/ |_/\\___/_/|_|\\____/  |"
    putStrLn "|     Welcome! Su's Neko v1.0         |"
    putStrLn "---------------------------------------"
    file <- readFile "testfile.ss"
    case parse file of
        Left s -> error s 
        Right sents -> do env <- setupEnvironment
                          ans <- analyzeSentences sents env succ fail
                          print ans
                          writeFile "output.ss" (showSentences sents)
    where
        succ :: Succeed
        succ v fail = return v
        fail :: Fail
        fail = error "Fail"


