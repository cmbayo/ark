type typ = Int | Bool | String
type operator = Add | Subtract | Multiply | Divide | Power | Equal | Neq | Less | Greater | LessEqual | GreaterEqual | And | Or



type expr = 
  | Binop of expr * operator * expr
  | IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | Id of string
  | Assign of string * expr
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list
  | Return of expr

  (* int x: name binding *)
type bind = typ * string


(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}
(* struct books(int a. int b. )*)
type struct_def = {
    structname: string;
    variables: bind list;
}

type program = struct_def list * (bind list * func_def list)
