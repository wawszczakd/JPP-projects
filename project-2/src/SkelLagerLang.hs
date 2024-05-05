-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelLagerLang where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsLagerLang

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsLagerLang.Ident -> Result
transIdent x = case x of
  AbsLagerLang.Ident string -> failure x

transProgram :: Show a => AbsLagerLang.Program' a -> Result
transProgram x = case x of
  AbsLagerLang.Prog _ topdefs -> failure x

transTopDef :: Show a => AbsLagerLang.TopDef' a -> Result
transTopDef x = case x of
  AbsLagerLang.FnDef _ type_ ident args block -> failure x
  AbsLagerLang.VarDef _ type_ ident -> failure x
  AbsLagerLang.VarDefAss _ type_ ident expr -> failure x

transArg :: Show a => AbsLagerLang.Arg' a -> Result
transArg x = case x of
  AbsLagerLang.ValArg _ type_ ident -> failure x
  AbsLagerLang.RefArg _ type_ ident -> failure x

transBlock :: Show a => AbsLagerLang.Block' a -> Result
transBlock x = case x of
  AbsLagerLang.Blk _ stmts -> failure x

transStmt :: Show a => AbsLagerLang.Stmt' a -> Result
transStmt x = case x of
  AbsLagerLang.Empty _ -> failure x
  AbsLagerLang.BStmt _ block -> failure x
  AbsLagerLang.DStmt _ topdef -> failure x
  AbsLagerLang.Ass _ ident expr -> failure x
  AbsLagerLang.Ret _ expr -> failure x
  AbsLagerLang.VRet _ -> failure x
  AbsLagerLang.Cond _ expr block -> failure x
  AbsLagerLang.CondElse _ expr block1 block2 -> failure x
  AbsLagerLang.While _ expr block -> failure x
  AbsLagerLang.SExp _ expr -> failure x
  AbsLagerLang.Break _ -> failure x
  AbsLagerLang.Continue _ -> failure x
  AbsLagerLang.Print _ expr -> failure x

transType :: Show a => AbsLagerLang.Type' a -> Result
transType x = case x of
  AbsLagerLang.Int _ -> failure x
  AbsLagerLang.Str _ -> failure x
  AbsLagerLang.Bool _ -> failure x
  AbsLagerLang.Void _ -> failure x
  AbsLagerLang.Fun _ type_ types -> failure x

transExpr :: Show a => AbsLagerLang.Expr' a -> Result
transExpr x = case x of
  AbsLagerLang.EVar _ ident -> failure x
  AbsLagerLang.ELitInt _ integer -> failure x
  AbsLagerLang.ELitTrue _ -> failure x
  AbsLagerLang.ELitFalse _ -> failure x
  AbsLagerLang.EApp _ ident exprs -> failure x
  AbsLagerLang.EString _ string -> failure x
  AbsLagerLang.Neg _ expr -> failure x
  AbsLagerLang.Not _ expr -> failure x
  AbsLagerLang.EMul _ expr1 mulop expr2 -> failure x
  AbsLagerLang.EAdd _ expr1 addop expr2 -> failure x
  AbsLagerLang.ERel _ expr1 relop expr2 -> failure x
  AbsLagerLang.EAnd _ expr1 expr2 -> failure x
  AbsLagerLang.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => AbsLagerLang.AddOp' a -> Result
transAddOp x = case x of
  AbsLagerLang.Plus _ -> failure x
  AbsLagerLang.Minus _ -> failure x

transMulOp :: Show a => AbsLagerLang.MulOp' a -> Result
transMulOp x = case x of
  AbsLagerLang.Times _ -> failure x
  AbsLagerLang.Div _ -> failure x
  AbsLagerLang.Mod _ -> failure x

transRelOp :: Show a => AbsLagerLang.RelOp' a -> Result
transRelOp x = case x of
  AbsLagerLang.LTH _ -> failure x
  AbsLagerLang.LE _ -> failure x
  AbsLagerLang.GTH _ -> failure x
  AbsLagerLang.GE _ -> failure x
  AbsLagerLang.EQU _ -> failure x
  AbsLagerLang.NE _ -> failure x
