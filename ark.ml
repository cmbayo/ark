open Ast

let binop (left: int) (operator: Ast.operator) (right: int): int =
  match operator with
  Add -> left + right
  | Subtract -> left - right
  | Multiply -> left * right
  | Divide -> left / right

let rec exec (program: Ast.program): int = 
  match program with
  | Binop(left, operator, right) -> 
    let left = exec left in
    let right = exec right in
    binop left operator right
  | IntLiteral(value) -> value
  | BoolLiteral(value) -> if value then 1 else 0

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  let result = exec program in
  print_endline (string_of_int result)