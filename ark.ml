open Sast

let rec power (base: int) (exponent: int): int =
  if exponent = 0 then 1
  else base * (power base (exponent - 1))

let int_binop (left: int) (operator: Ast.operator) (right: int): int =
  match operator with
  Add -> left + right
  | Subtract -> left - right
  | Multiply -> left * right
  | Divide -> left / right
  | Power -> power left right

let rec int_eval (sexpr: Sast.sexpr): int =
  match sexpr with
  (Int, SBinop(left, operator, right)) -> 
    let left = int_eval left in
    let right = int_eval right in
    int_binop left operator right
  | (Int, SIntLiteral(value)) -> value
  | (Int, SAssign(var_type, var, s)) -> 
    let result = int_eval s in
    result
  | _ -> raise (Failure "Fatal error.")

let rec bool_eval (sexpr: Sast.sexpr): bool =
  match sexpr with
  (Bool, SBoolLiteral(value)) -> value
  | (Bool, SAssign(var_type, var, s)) ->
    let result = bool_eval s in
    result
  | _ -> raise (Failure "Fatal error.")

let rec eval (sexpr: Sast.sexpr) =
  match sexpr with
  (Int, _) as int_sexpr -> 
    let result = int_eval int_sexpr in
    Printf.printf "%d\n" result; None
  | (Bool, _) as bool_sexpr ->
    let result = bool_eval bool_sexpr in
    if result then 
      let _ = Printf.printf "true\n" in
      None
    else 
      let _ = Printf.printf "false\n" in
      None 

let exec (sprogram: Sast.sprogram) = 
  match sprogram with
  SExpr(sexpr) -> None
  | SPrint(sexpr) -> eval sexpr

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  let sprogram = Semantics.check program in
  Irgen2.translate(sprogram)