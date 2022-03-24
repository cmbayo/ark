type operator = Add | Subtract | Multiply | Divide


type expr = 
    | Binop of expr * operator * expr
    | IntLiteral of int
    | BoolLiteral of bool

type program = expr