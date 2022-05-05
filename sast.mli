open Ast

type sexpr = typ * sx
and sx = 
    SBinop of sexpr * operator * sexpr
    | SIntLiteral of int
    | SBoolLiteral of bool
    | SId of string
    | SAssign of typ * string * sexpr
    | SCall of string * sexpr list

type sstmt =
    | SPrint of sexpr
    | SExpr of sexpr
    | SBlock of sstmt list
    | SIf of sexpr * sstmt * sstmt
    | SWhile of sexpr * sstmt
    | SREturn of sexpr

type sfunc_dec = {
    srtyp: typ;
    sfname: string;
    sparameters: bind list; (* a list of parameters*)
    slocals: bind list; (* local varibles*)
    sbody: sstmt list;
}


type sprogram = bind list * sfunc_dec list