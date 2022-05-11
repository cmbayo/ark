open Ast

type sexpr = typ * sx
and sx =
  SBinop of sexpr * operator * sexpr
  | SIntLiteral of int
  | SBoolLiteral of bool
  | SAssign of typ * string * sexpr
  | SCall of string * sexpr list

type sstmt =
  SExpr of sexpr
  | SBlock of sstmt list
  | SReturn of sexpr


type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list