type typ = Int | Bool
type operator = Add | Subtract | Multiply | Divide | Power| Equal | Neq | Less | And | Or

type expr = 
      Binop of expr * operator * expr
    | IntLiteral of int
    | BoolLiteral of bool
    | Id of string
    | Assign of string * expr
    | Call of string * expr list
type stmt =
    | Print of expr
    | Expr of expr
    | Block of stmt list
    | If of expr * stmt * stmt
    | While of expr * stmt
    | Return of expr

type bind = typ * string

type func_dec = {
    rtyp: typ;
    fname: string;
    parameters: bind list; (* a list of parameters*)
    locals: bind list; (* local varibles*)
    body: stmt list;
}

type program = bind list * func_dec list (* program now is a bunch of global variable declarations
                                         and function declarations*)