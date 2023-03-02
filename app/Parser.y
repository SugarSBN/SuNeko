{
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Lexer
import Frontend.Syntax
import Control.Monad.Except

}

-- Entry point
%name sentences

-- Entry point
%name sentences

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let    { TokenLet }
    True   { TokenTrue }
    False  { TokenFalse }
    if     { TokenIf}
    then   { TokenThen }
    else   { TokenElse }
    cond   { TokenCond}
    define { TokenDefine}
    NUM    { TokenNum $$ }
    lambda { TokenLambda }
    begin  { TokenBegin }
    set    { TokenSet }

    '+'   { TokenAdd }
    '-'   { TokenSub }
    '*'   { TokenMul }
    '/'   { TokenDiv }
    '%'   { TokenMod }
    '=='  { TokenEq  }
    '!='  { TokenNeq }   
    '<'   { TokenLt }
    '>'   { TokenGt } 
    '<='  { TokenLe }
    '>='  { TokenGe }
    '&&'  { TokenAnd }
    '||'  { TokenOr }
    '='   { TokenAss }
    ':='  { TokenColEq }
    '=>'  { TokenRightarrow}
    
    ';'   { TokenSemi }

    '('   { TokenLParen }
    ')'   { TokenRParen }
    '{'   { TokenLBrace }
    '}'   { TokenRBrace } 
    '['   { TokenLBracket }
    ']'   { TokenRBracket }
    '"'   { TokenQuote }
    VAR    { TokenSym $$ }

-- Operators
%nonassoc '=' '=>' '"'
%nonassoc '(' ')' '{' '}' '[' ']'
%left '==' '<=' '>=' '!=' '<' '>'
%left '+' '-'
%left '*' '/' '%'
%nonassoc let True False if then else cond define NUM lambda begin set VAR
%left APP
%%

Sentences : Sentence                                         { [$1] }
          | Sentence Sentences                               { $1 : $2 }

Sentence : Definition                                        { Definition $1 }
         | Expression                                        { Expression $1 }

Definition : '{' define VAR ':=' Expression '}'                   { ExprDefine $3 $5 }
           | '{' define '(' VAR ')' ':=' Expression '}'           { FuncDefine $4 [] $7 }
           | '{' define '(' VAR variables ')' ':=' Expression '}' { FuncDefine $4 $5 $8}

variables : VAR                                              { [$1] }
          | VAR variables                                    { $1 : $2 }

Bindings : VAR '=' Expression                                { [($1, $3)] }
         | VAR '=' Expression Bindings                       { ($1, $3) : $4 }

CondBindings : Expression '=>' Expression                    { [($1, $3)] }
             | Expression '=>' Expression CondBindings       { ($1, $3) : $4 }
             | else '=>' Expression                          { [(ExprVar "else", $3)] }

Expressions : Expression                                     { [$1] }
            | Expression Expressions                         { $1 : $2 }

Expression : '(' Expression ')'                              { $2 }
           | True                                            { ExprInt 1 }
           | False                                           { ExprInt 0 }
           | NUM                                             { ExprInt $1 }
           | VAR                                             { ExprVar $1 }
           | lambda '(' variables ')' Expression             { ExprLambda $3 $5 }
           | if Expression then Expression else Expression   { ExprIf $2 $4 $6 }
           | begin '[' Expressions ']'                       { ExprBegin $3 }
           | let '[' Bindings ']' Expression                 { ExprLet $3 $5 }
           | cond '[' CondBindings ']'                       { ExprCond $3 }
           | '(' Expressions ')'                             { ExprApp $2 }
           | Expression '+' Expression                       { ExprAdd $1 $3 }
           | Expression '-' Expression                       { ExprSub $1 $3 }
           | Expression '*' Expression                       { ExprMul $1 $3 }
           | Expression '/' Expression                       { ExprDiv $1 $3 }
           | Expression '%' Expression                       { ExprMod $1 $3 }
           | Expression '==' Expression                      { ExprEq $1 $3 }
           | Expression '!=' Expression                      { ExprNeq $1 $3 }
           | Expression '<' Expression                       { ExprLt $1 $3 }
           | Expression '>' Expression                       { ExprGt $1 $3 }
           | Expression '<=' Expression                      { ExprLe $1 $3 }
           | Expression '>=' Expression                      { ExprGe $1 $3 }
           | Expression '&&' Expression                      { ExprAnd $1 $3 }
           | Expression '||' Expression                      { ExprOr $1 $3 }
           | set  VAR Expression                             { ExprSet $2 $3 }

{

parseError :: [Token] -> Except String a
parseError (l : ls) = throwError (show (l : ls))
parseError [] = throwError "Unexpected end of Input"

parse :: String -> Either String Sentences
parse input = runExcept $ do
    tokenStream <- scanTokens input
    sentences tokenStream

}