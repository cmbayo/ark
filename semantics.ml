open Ast
open Sast

module StringMap = Map.Make(String)

let check (prog: program): sprogram =
  let rec check_expr(ex: expr): sexpr =
    match ex with
    Binop(e1, op, e2) ->
      let (t1, e1') = check_expr e1 in
      let (t2, e2') = check_expr e2 in
      if t1 = t2 then
        let t = match op with
          Add | Subtract | Multiply | Divide | Power when t1 = Int -> Int
          | _ -> raise (Failure "Fatal error.")
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure "Fatal error.") 
    | IntLiteral(x) -> (Int, SIntLiteral x)
    | BoolLiteral(x) -> (Bool, SBoolLiteral x)
    | Assign(var_type, var, e) -> 
      let (t, e') = check_expr e in
      if t = var_type then 
        (var_type, SAssign(var_type, var, (var_type, e')))
      else raise (Failure "Fatal error.")
  in

  let rec check_stmt(st: stmt): sstmt = 
    match st with
    Expr(ex) -> SExpr(check_expr ex)
    | Print(ex) -> SPrint(check_expr ex)
    | Block(s) -> SBlock(check_stmt s)
    | If(e, s1, s2) -> 
      let (t, e') = check_expr e in
      (match t with
      bool -> SIf(check_expr e, check_stmt s1, check_stmt s2))
  in

  check_stmt(prog)