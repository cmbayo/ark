open Ast

type sexpr = typ * sx
and sx =
  SBinop of sexpr * operator * sexpr
  | SIntLiteral of int
  | SBoolLiteral of bool
  | SStringLiteral of string
  | SListLiteral of sexpr list
  | SListPattern of string * string
  | SId of string
  | SAssign of string * sexpr
  | SCall of string * sexpr list

type sstmt =
  SExpr of sexpr
  | SBlock of sstmt list
  | SIf of sexpr * sstmt list * sstmt list
  | SWhile of sexpr * sstmt list
  | SReturn of sexpr


type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list
