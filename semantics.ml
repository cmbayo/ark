open Ast
open Sast

module StringMap = Map.Make(String)

let check (globals, functions) =

  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      parameters = [(Int, "x")];
      locals = []; body = [] } StringMap.empty
  in
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  let function_decls = List.fold_left add_func built_in_decls functions
  in
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "parameter" func.parameters;
    check_binds "local" func.locals;
  
  let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.parameters @ func.locals )
    in
  
  let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in


  let rec check_expr = function 
      IntLiteral l -> (Int, SIntLiteral l)
    | BoolLiteral l -> (Bool, SBoolLiteral l)
    | Id var -> (type_of_identifier var, SId var)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var
      and (rt, e') = check_expr e in
      let err = "illegal assignment "
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))

    | Binop(e1, op, e2) as e->
      let (t1, e1') = check_expr e1 in
      let (t2, e2') = check_expr e2 in
      if t1 = t2 then
        let t = match op with
          Add | Subtract | Multiply | Divide | Power when t1 = Int -> Int
          | Equal | Neq -> Bool
          | Less when t1 = Int -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure "Fatal error.")
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure "Fatal error.") 

    | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.parameters in
        if List.length args != param_length then
          raise (Failure ("error"))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found "
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.parameters args
          in (fd.rtyp, SCall(fname, args'))
    in
  
  let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in "))
    in

  let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
  and check_stmt(st: stmt): sstmt = 
    match st with
    Expr(ex) -> SExpr(check_expr ex)
    | Print(ex) -> SPrint(check_expr ex)
    | Block(s) -> SBlock(check_stmt_list s)
    | If(e, st1, st2) ->
      SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
    | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
    | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return error"))
  in
  { srtyp = func.rtyp;
      sfname = func.fname;
      sparameters = func.parameters;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
in
(globals, List.map check_func functions)
