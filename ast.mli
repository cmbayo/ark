type typ = Int | Bool
type operator = Add | Subtract | Multiply | Divide | Power

type expr = 
    | Binop of expr * operator * expr
    | IntLiteral of int
    | BoolLiteral of bool
    | Assign of typ * string * expr
    | Call of string * expr list
type stmt =
    | Print of expr
    | Expr of expr
    | Block of stmt
    | If of expr * stmt * stmt

type program = stmt