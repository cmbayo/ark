type typ = Int | Bool | String | Struct of string
type operator = Add | Subtract | Multiply | Divide | Power | Equal | Neq | Less | Greater | LessEqual | GreaterEqual | And | Or
(* int x: name binding *)
and bind = typ * string
and struct_decl = {
        sname: string;
        svariables: bind list;
}

and expr = 
  | Binop of expr * operator * expr
  | IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | Id of string
  | Assign of string * expr
  | StructAssign of string * string * expr
  | StructGet of expr * expr
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt 
  | While of expr * stmt
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
