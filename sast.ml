open Ast

type sexpr = typ * sx
and sx =
  SBinop of sexpr * operator * sexpr
  | SIntLiteral of int
  | SBoolLiteral of bool
  | SStringLiteral of string
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

type sstruct_def = {
    sstructname: string;
    svariables: bind list;
}

type sprogram = sstruct_def *(bind list * sfunc_def list)
