open Ast

let rec power (base: int) (exponent: int): int =
  if exponent = 0 then 1
  else base * (power base (exponent - 1))

let binop (left: int) (operator: Ast.operator) (right: int): int =
  match operator with
  Add -> left + right
  | Subtract -> left - right
  | Multiply -> left * right
  | Divide -> left / right
  | Power -> power left right

let rec eval (expr: Ast.expr): int =
  match expr with
  | Binop(left, operator, right) -> 
    let left = eval left in
    let right = eval right in
    binop left operator right
  | IntLiteral(value) -> value 
  | BoolLiteral(value) -> if value then 1 else 0

let rec exec (program: Ast.program) = 
  match program with
  | Expr(expr) -> None
  | Print(expr) -> 
    let result = eval expr in
    print_endline (string_of_int result);
    None

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  exec program