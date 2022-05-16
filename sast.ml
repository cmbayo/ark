open Ast
(*
type sstruct_decl = SStructDecl of string * bind list
*)
type sexpr = typ * sx


and sx =
  SBinop of sexpr * operator * sexpr
  | SIntLiteral of int
  | SBoolLiteral of bool
  | SStringLiteral of string
  | SId of string
  | SAssign of string * sexpr
  | SStructAssign of string * string * sexpr
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


type sstruct_decl = {
    ssname: string;
    ssvariables: bind list;
}

type sprogram = sstruct_decl list *(bind list * sfunc_def list)
