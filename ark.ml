open Ast

let exec (program: Ast.program) = 
  match program with
  IntLiteral(value) -> print_endline (string_of_int value)
  | BoolLiteral(value) -> print_endline "Boolean"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  exec program