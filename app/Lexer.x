{

{-# LANGUAGE FlexibleContexts #-}
module Lexer where
import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

    -- Whitespace insensitive
    $eol                          ;
    $white+                       ;

    -- Comments
    ";".*                        ;

    -- Syntax
    let                           { \s -> TokenLet   }
    True                          { \s -> TokenTrue  }
    False                         { \s -> TokenFalse }
    if                            { \s -> TokenIf    }
    then                          { \s -> TokenThen  }
    else                          { \s -> TokenElse  }
    cond                          { \s -> TokenCond  }
    define                        { \s -> TokenDefine  }
    $digit+                       { \s -> TokenNum (read s) }
    lambda                        { \s -> TokenLambda }
    begin                         { \s -> TokenBegin }
    set                           { \s -> TokenSet }

    "+"                           { \s -> TokenAdd }
    "-"                           { \s -> TokenSub }
    "*"                           { \s -> TokenMul }
    "/"                           { \s -> TokenDiv }
    "%"                           { \s -> TokenMod }
    "=="                          { \s -> TokenEq }
    "!="                          { \s -> TokenNeq }
    "<"                           { \s -> TokenLt }
    ">"                           { \s -> TokenGt }
    "<="                          { \s -> TokenLe }
    ">="                          { \s -> TokenGe }
    "&&"                          { \s -> TokenAnd }
    "||"                          { \s -> TokenOr }
    "="                           { \s -> TokenAss }
    ":="                          { \s -> TokenColEq}

    "=>"                          { \s -> TokenRightarrow}
    
    ";"                           { \s -> TokenSemi }
    "("                           { \s -> TokenLParen }
    ")"                           { \s -> TokenRParen }
    "{"                           { \s -> TokenLBrace }
    "}"                           { \s -> TokenRBrace }
    "["                           { \s -> TokenLBracket }
    "]"                           { \s -> TokenRBracket }
    "\""                          { \s -> TokenQuote }
    $alpha [$alpha $digit \_ \' \- \> \< \? \! \_]*   { \s -> TokenSym s }

{

data Token 
  = TokenLet
  | TokenTrue
  | TokenFalse
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenCond
  | TokenDefine
  | TokenNum Int
  | TokenLambda
  | TokenBegin
  | TokenSet
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenMod
  | TokenEq
  | TokenNeq
  | TokenLt
  | TokenGt
  | TokenLe
  | TokenGe
  | TokenAnd
  | TokenOr
  | TokenAss
  | TokenColEq
  | TokenRightarrow
  | TokenSemi
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenLBracket
  | TokenRBracket
  | TokenQuote
  | TokenSym String
  deriving (Eq, Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) 
    where 
        go inp@(_,_bs,str) = case alexScan inp 0 of
                                AlexEOF -> return []
                                AlexError _ -> throwError "Invalid lexeme."
                                AlexSkip  inp' len     -> go inp'
                                AlexToken inp' len act -> do res <- go inp'
                                                             let rest = act (take len str)
                                                             return (rest : res)
}