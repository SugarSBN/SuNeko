{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Backend.Interpreter where

import Frontend.Syntax
import Backend.Environment
import Control.Monad

type Fail = IO ()
type Succeed = Expression -> Fail -> IO Expression

type Analyze = Environment -> Succeed -> Fail -> IO Expression

analyzeExpr :: Expression -> Analyze
analyzeExpr (ExprInt n) env succ fail = succ (ExprInt n) fail
analyzeExpr (ExprVar var) env succ fail = do
    val <- lookUpVariable var env
    succ val fail

---------------------------------------------------------------------------------------------------------------------------------

analyzeExpr (ExprLambda vars expr) env succ fail =
    if null vars then analyzeExpr expr env succ fail else
        analyzeExpr expr env (\v fail1 -> succ (ExprLambda vars v) fail1) fail

analyzeExpr (ExprBegin exprs) env succ fail
    | null exprs = error "Empty begin"
    | otherwise = loop (head procs) (tail procs) env succ fail
    where
        sequence :: Analyze -> Analyze -> Analyze
        sequence a b env succ = a env (\v1 fail1 -> b env succ fail1)

        loop :: Analyze -> [Analyze] -> Analyze
        loop first rest = if null rest then first else loop (sequence first (head rest)) (tail rest)

        procs = map analyzeExpr exprs

analyzeExpr (ExprIf cond thenExpr elseExpr) env succ fail = do
    pred env (\v fail1 -> (if v == ExprInt 0 then elseProc env succ fail1 else thenProc env succ fail1)) fail
    where
        pred = analyzeExpr cond
        thenProc = analyzeExpr thenExpr
        elseProc = analyzeExpr elseExpr

analyzeExpr (ExprCond cons) env succ fail =
    analyzeExpr (condToIf cons) env succ fail
    where
        condToIf :: [(Expression, Expression)] -> Expression
        condToIf [] = error "Empty cond"
        condToIf conds = if cond == ExprVar "else" then expr else ExprIf cond expr (condToIf tl)
            where
                hd = head conds
                tl = tail conds
                cond = fst hd
                expr = snd hd

analyzeExpr (ExprSet var expr) env succ fail = vproc env (\v1 fail1 -> do
    oldValue <- lookUpVariable var env
    setVariableValue var v1 env
    succ (ExprVar $ var ++ " Set Done.") (do
        setVariableValue var oldValue env
        fail1)
    ) fail
    where
        vproc = analyzeExpr expr

analyzeExpr (ExprApp (p : args)) env succ fail = 
    if null args then analyzeExpr p env succ fail else
        analyzeExpr p env (\v fail1 -> do
            case v of
                (ExprLambda paras body) -> do
                    getArgs rua env (\vals fail2 -> do newEnv <- extendEnvironment paras vals env
                                                       analyzeExpr body newEnv succ fail2) fail1
                _ -> error "<App>: Not a Lambda!"
            ) fail
    where
        rua = map analyzeExpr args
        getArgs [] env succ fail = succ [] fail
        getArgs (a : as) env succ fail = a env (\v1 fail1 -> getArgs as env (\vs fail2 -> succ (v1 : vs) fail2) fail1) fail

analyzeExpr (ExprLet binds expr) env succ fail = analyzeExpr (ExprApp (ExprLambda vars expr : vals)) env succ fail
    where
        vars = map fst binds
        vals = map snd binds
        
---------------------------------------------------------------------------------------------------------------------------------

analyzeExpr (ExprAdd e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (n1 + n2)
        (e1, e2) -> ExprAdd e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprSub e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (n1 - n2)
        (e1, e2) -> ExprSub e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprMul e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (n1 * n2)
        (e1, e2) -> ExprMul e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprDiv e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (n1 `div` n2)
        (e1, e2) -> ExprDiv e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprMod e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (n1 `mod` n2)
        (e1, e2) -> ExprMod e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprEq e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 == n2 then 1 else 0)
        (e1, e2) -> ExprEq e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprNeq e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 /= n2 then 1 else 0)
        (e1, e2) -> ExprNeq e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprLt e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 < n2 then 1 else 0)
        (e1, e2) -> ExprLt e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprGt e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 > n2 then 1 else 0)
        (e1, e2) -> ExprGt e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprLe e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 <= n2 then 1 else 0)
        (e1, e2) -> ExprLe e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprGe e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 >= n2 then 1 else 0)
        (e1, e2) -> ExprGe e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprAnd e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 /= 0 && n2 /= 0 then 1 else 0)
        (e1, e2) -> ExprAnd e1 e2
    ) fail2) fail1) fail
analyzeExpr (ExprOr e1 e2) env succ fail = analyzeExpr e1 env (\v1 fail1 -> analyzeExpr e2 env (\v2 fail2 -> succ (
    case (v1, v2) of
        (ExprInt n1, ExprInt n2) -> ExprInt (if n1 /= 0 || n2 /= 0 then 1 else 0)
        (e1, e2) -> ExprOr e1 e2
    ) fail2) fail1) fail

---------------------------------------------------------------------------------------------------------------------------------

analyzeDefine :: Definition -> Analyze
analyzeDefine (ExprDefine var val) env succ fail = do
    analyzeExpr val env (\v fail1 -> do defineVariable var v env
                                        succ (ExprVar $ var ++ " Expression Definition Done.") fail1) fail

analyzeDefine (FuncDefine name params body) env succ fail = do let exp = ExprLambda params body
                                                               defineVariable name exp env
                                                               succ (ExprVar $ name ++ " Function Definition Done.") fail

---------------------------------------------------------------------------------------------------------------------------------

analyzeSentence :: Sentence -> Analyze
analyzeSentence (Definition s) = analyzeDefine s
analyzeSentence (Expression s) = analyzeExpr s

analyzeSentences :: Sentences -> Analyze
analyzeSentences sentences env succ fail
    | null sentences = error "Empty Program!"
    | otherwise = loop (head sents) (tail sents) env succ fail
    where
        sequence :: Analyze -> Analyze -> Analyze
        sequence a b env succ = a env (\v1 fail1 -> do print v1
                                                       b env succ fail1)

        loop :: Analyze -> [Analyze] -> Analyze
        loop first rest = if null rest then first else loop (sequence first (head rest)) (tail rest)

        sents = map analyzeSentence sentences
