type operator = Add | Subtract | Multiply | Divide | Power

type expr = 
    | Binop of expr * operator * expr
    | IntLiteral of int
    | BoolLiteral of bool

type stmt =
    | Print of expr
    | Expr of expr

type program = stmt