type typ = Int | Bool | String | Struct of string
type operator = Add | Subtract | Multiply | Divide | Power | Equal | Neq | Less | Greater | LessEqual | GreaterEqual | And | Or
(* int x: name binding *)
type bind = typ * string
type struct_decl = {
        sname: string;
        svariables: bind list;
}

type expr = 
  | Binop of expr * operator * expr
  | IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | Id of string
  | Assign of string * expr
  | StructAssign of string * string * expr
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | Return of expr




(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}


type program = struct_decl list * (bind list * func_def list)
