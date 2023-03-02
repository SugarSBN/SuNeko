
module Frontend.Syntax where

type Variable = String

type Sentences = [Sentence]
showSentences :: Sentences -> String
showSentences = unlines . map show

data Sentence = Definition Definition | Expression Expression
    deriving (Eq)

instance Show Sentence where
    show (Definition def) = show def
    show (Expression expr) = show expr

data Definition = ExprDefine Variable Expression
                | FuncDefine Variable [Variable] Expression
    deriving (Eq)

instance Show Definition where
    show (ExprDefine v expr) = "{define " ++ v ++ " " ++ show expr ++ "}"
    show (FuncDefine v vars expr) = "{define " ++ v ++ " (" ++ unwords vars ++ ") " ++ show expr ++ "}"

data Expression = ExprInt Int
                | ExprVar Variable
                ----------------------------------------------
                | ExprLambda [Variable] Expression
                | ExprIf Expression Expression Expression
                | ExprBegin [Expression]
                | ExprSet Variable Expression
                | ExprLet [(Variable, Expression)] Expression
                | ExprCond [(Expression, Expression)]
                | ExprApp [Expression]
                ----------------------------------------------
                | ExprAdd Expression Expression
                | ExprSub Expression Expression
                | ExprMul Expression Expression
                | ExprDiv Expression Expression
                | ExprMod Expression Expression
                | ExprEq Expression Expression
                | ExprNeq Expression Expression
                | ExprLt Expression Expression
                | ExprGt Expression Expression
                | ExprLe Expression Expression
                | ExprGe Expression Expression
                | ExprAnd Expression Expression
                | ExprOr Expression Expression
    deriving (Eq)

instance Show Expression where
    show (ExprInt n) = show n
    show (ExprVar v) = v
    show (ExprLambda vars expr) = "(lambda (" ++ unwords vars ++ ") " ++ show expr ++ ")"
    show (ExprIf cond expr1 expr2) = "(if " ++ show cond ++ " " ++ show expr1 ++ " " ++ show expr2 ++ ")"
    show (ExprBegin exprs) = "(begin " ++ unwords (map show exprs) ++ ")"
    show (ExprSet var expr) = "(set " ++ var ++ " " ++ show expr ++ ")"
    show (ExprLet bindings expr) = "(let [" ++ unwords (map (\(var, expr) -> "(" ++ var ++ " " ++ show expr ++ ")") bindings) ++ "] " ++ show expr ++ ")"
    show (ExprCond cases) = "(cond [" ++ unwords (map (\(cond, expr) -> "(" ++ show cond ++ " => " ++ show expr ++ ")") cases) ++ "])"
    show (ExprApp exprs) = "(" ++ unwords (map show exprs) ++ ")"
    show (ExprAdd expr1 expr2) = "(" ++ show expr1 ++ " + " ++ show expr2 ++ ")"
    show (ExprSub expr1 expr2) = "(" ++ show expr1 ++ " - " ++ show expr2 ++ ")"
    show (ExprMul expr1 expr2) = "(" ++ show expr1 ++ " * " ++ show expr2 ++ ")"
    show (ExprDiv expr1 expr2) = "(" ++ show expr1 ++ " / " ++ show expr2 ++ ")"
    show (ExprMod expr1 expr2) = "(" ++ show expr1 ++ " % " ++ show expr2 ++ ")"
    show (ExprEq expr1 expr2) = "(" ++ show expr1 ++ " == " ++ show expr2 ++ ")"
    show (ExprNeq expr1 expr2) = "(" ++ show expr1 ++ " != " ++ show expr2 ++ ")"
    show (ExprLt expr1 expr2) = "(" ++ show expr1 ++ " < " ++ show expr2 ++ ")"
    show (ExprGt expr1 expr2) = "(" ++ show expr1 ++ " > " ++ show expr2 ++ ")"
    show (ExprLe expr1 expr2) = "(" ++ show expr1 ++ " <= " ++ show expr2 ++ ")"
    show (ExprGe expr1 expr2) = "(" ++ show expr1 ++ " >= " ++ show expr2 ++ ")"
    show (ExprAnd expr1 expr2) = "(" ++ show expr1 ++ " && " ++ show expr2 ++ ")"
    show (ExprOr expr1 expr2) = "(" ++ show expr1 ++ " || " ++ show expr2 ++ ")"