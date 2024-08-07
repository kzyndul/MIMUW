-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language grammar.

module AbsGrammar where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = PProgram [Fn]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Fn = FnDef Type Ident [Arg] Block
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = AArg Type Ident | RefArg Type Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block = Block [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = BStmt Block
    | Decl Type [Item]
    | SExp Expr
    | Ass Ident Expr
    | Ret Expr
    | ListPush Ident Expr
    | ListPop Ident
    | Print Expr
    | Cond Expr Block
    | CondElse Expr Block Block
    | Break
    | Continue
    | While Expr Block
    | FnDecl Fn
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = Int | Str | Bool | TList Type | Fun Type [TypeOrRef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TypeOrRef = TypeOrRefType Type | TypeOrRefRef Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EVar Ident
    | EListEmpty Type [Expr]
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EString String
    | EApp Expr [ExprOrRef]
    | EListLength Ident
    | EListAt Ident Expr
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ExprOrRef = ERExpr Expr | ERRef Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AddOp = Plus | Minus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MulOp = Times | Div
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

