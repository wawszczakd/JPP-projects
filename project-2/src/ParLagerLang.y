-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParLagerLang
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsLagerLang
import LexLagerLang

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'        { PT _ (TS _ 1)  }
  '!='       { PT _ (TS _ 2)  }
  '%'        { PT _ (TS _ 3)  }
  '&'        { PT _ (TS _ 4)  }
  '&&'       { PT _ (TS _ 5)  }
  '('        { PT _ (TS _ 6)  }
  ')'        { PT _ (TS _ 7)  }
  '*'        { PT _ (TS _ 8)  }
  '+'        { PT _ (TS _ 9)  }
  ','        { PT _ (TS _ 10) }
  '-'        { PT _ (TS _ 11) }
  '/'        { PT _ (TS _ 12) }
  ';'        { PT _ (TS _ 13) }
  '<'        { PT _ (TS _ 14) }
  '<='       { PT _ (TS _ 15) }
  '='        { PT _ (TS _ 16) }
  '=='       { PT _ (TS _ 17) }
  '>'        { PT _ (TS _ 18) }
  '>='       { PT _ (TS _ 19) }
  'bool'     { PT _ (TS _ 20) }
  'break'    { PT _ (TS _ 21) }
  'continue' { PT _ (TS _ 22) }
  'else'     { PT _ (TS _ 23) }
  'false'    { PT _ (TS _ 24) }
  'if'       { PT _ (TS _ 25) }
  'int'      { PT _ (TS _ 26) }
  'print'    { PT _ (TS _ 27) }
  'return'   { PT _ (TS _ 28) }
  'string'   { PT _ (TS _ 29) }
  'true'     { PT _ (TS _ 30) }
  'void'     { PT _ (TS _ 31) }
  'while'    { PT _ (TS _ 32) }
  '{'        { PT _ (TS _ 33) }
  '||'       { PT _ (TS _ 34) }
  '}'        { PT _ (TS _ 35) }
  L_Ident    { PT _ (TV _)    }
  L_integ    { PT _ (TI _)    }
  L_quoted   { PT _ (TL _)    }

%%

Ident :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Ident) }
Ident  : L_Ident { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Ident (tokenText $1)) }

Integer :: { (AbsLagerLang.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (AbsLagerLang.BNFC'Position, String) }
String   : L_quoted { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Program) }
Program
  : ListTopDef { (fst $1, AbsLagerLang.Prog (fst $1) (snd $1)) }

TopDef :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.TopDef) }
TopDef
  : Type Ident '(' ListArg ')' Block { (fst $1, AbsLagerLang.FnDef (fst $1) (snd $1) (snd $2) (snd $4) (snd $6)) }
  | Type Ident ';' { (fst $1, AbsLagerLang.VarDef (fst $1) (snd $1) (snd $2)) }
  | Type Ident '=' Expr ';' { (fst $1, AbsLagerLang.VarDefAss (fst $1) (snd $1) (snd $2) (snd $4)) }

ListTopDef :: { (AbsLagerLang.BNFC'Position, [AbsLagerLang.TopDef]) }
ListTopDef
  : TopDef { (fst $1, (:[]) (snd $1)) }
  | TopDef ListTopDef { (fst $1, (:) (snd $1) (snd $2)) }

Arg :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Arg) }
Arg
  : Type Ident { (fst $1, AbsLagerLang.ValArg (fst $1) (snd $1) (snd $2)) }
  | Type '&' Ident { (fst $1, AbsLagerLang.RefArg (fst $1) (snd $1) (snd $3)) }

ListArg :: { (AbsLagerLang.BNFC'Position, [AbsLagerLang.Arg]) }
ListArg
  : {- empty -} { (AbsLagerLang.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Block :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Block) }
Block
  : '{' ListStmt '}' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Blk (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (AbsLagerLang.BNFC'Position, [AbsLagerLang.Stmt]) }
ListStmt
  : {- empty -} { (AbsLagerLang.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Stmt) }
Stmt
  : ';' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Empty (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, AbsLagerLang.BStmt (fst $1) (snd $1)) }
  | TopDef { (fst $1, AbsLagerLang.DStmt (fst $1) (snd $1)) }
  | Ident '=' Expr ';' { (fst $1, AbsLagerLang.Ass (fst $1) (snd $1) (snd $3)) }
  | 'return' Expr ';' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Ret (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.VRet (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'if' '(' Expr ')' Block { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Cond (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Block 'else' Block { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.CondElse (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Block { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.While (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | Expr ';' { (fst $1, AbsLagerLang.SExp (fst $1) (snd $1)) }
  | 'break' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Break (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'continue' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Continue (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'print' '(' Expr ')' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Print (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $3)) }

Type :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Type) }
Type
  : 'int' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Int (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Str (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'bool' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Bool (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'void' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Void (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }

ListType :: { (AbsLagerLang.BNFC'Position, [AbsLagerLang.Type]) }
ListType
  : {- empty -} { (AbsLagerLang.BNFC'NoPosition, []) }
  | Type { (fst $1, (:[]) (snd $1)) }
  | Type ',' ListType { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr6
  : Ident { (fst $1, AbsLagerLang.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, AbsLagerLang.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.ELitTrue (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.ELitFalse (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListExpr ')' { (fst $1, AbsLagerLang.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, AbsLagerLang.EString (fst $1) (snd $1)) }
  | '(' Expr ')' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr5
  : '-' Expr6 { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Neg (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr6 { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Not (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, AbsLagerLang.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, AbsLagerLang.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, AbsLagerLang.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, AbsLagerLang.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, AbsLagerLang.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (AbsLagerLang.BNFC'Position, [AbsLagerLang.Expr]) }
ListExpr
  : {- empty -} { (AbsLagerLang.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.AddOp) }
AddOp
  : '+' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Plus (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Minus (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.MulOp) }
MulOp
  : '*' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Times (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Div (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.Mod (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (AbsLagerLang.BNFC'Position, AbsLagerLang.RelOp) }
RelOp
  : '<' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.LTH (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.LE (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.GTH (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.GE (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.EQU (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1), AbsLagerLang.NE (uncurry AbsLagerLang.BNFC'Position (tokenLineCol $1))) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err AbsLagerLang.Program
pProgram = fmap snd . pProgram_internal
}

