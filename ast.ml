type typ = Int | Bool
type operator = Add | Subtract | Multiply | Divide | Power  



type expr = 
  | Binop of expr * operator * expr
  | IntLiteral of int
  | BoolLiteral of bool
  | Id of string
  | Assign of string * expr
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
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

type program = bind list * func_def list