open Ast

type sexpr = typ * sx
and sx = 
    SBinop of sexpr * operator * sexpr
    | SIntLiteral of int
    | SBoolLiteral of bool
    | SAssign of typ * string * sexpr

type sstmt =
    | SPrint of sexpr
    | SExpr of sexpr

type sprogram = sstmt