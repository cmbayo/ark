open Ast
open Sast

module StringMap = Map.Make(String)


let check (structs, (globals, functions)) =
  

  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  check_binds "global" globals;

  let built_in_structs = StringMap.empty

  in

  let add_struct smap sd = 
          let sdup_err = "duplicate structure " ^ sd.sname
          and smake_err er = raise (Failure er)
          and m = sd.sname
          in match sd with
           _ when StringMap.mem m smap -> smake_err sdup_err
         | _ -> StringMap.add m sd smap
  in
  let struct_decls = List.fold_left add_struct built_in_structs structs
  in


  let check_struct sd = 
    check_binds "variables" sd.svariables;
  in

(*
  let check_struct (kind: string) (variables: (typ * string * bind list) list) =
          (*print_string name;*)
          let rec dups = function
                  [] -> ()
                | ((_,n1, _) :: (_,n2,_) :: _) when n1 = n2 ->
                  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
                | _ :: t -> dups t
              in dups (List.sort (fun (_,a, _) (_,b,_) -> compare a b) variables)
  in

  check_struct "struct" structs;
*)
  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(Int, "x")];
      locals = []; body = [] } StringMap.empty
  in

  let built_in_decls =
     StringMap.add "printstring" {
       rtyp = String;
       fname = "printstring";
       formals = [(String, "y")];
       locals = []; body = [] } built_in_decls
  in


  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname 
    in match fd with 
  
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

  let _ = find_func "main" in 

  let check_func func =
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        IntLiteral l -> (Int, SIntLiteral l)
      | BoolLiteral l -> (Bool, SBoolLiteral l)
      | StringLiteral l -> (String, SStringLiteral l)
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var
        and (rt, e') = check_expr e in
        let err = "illegal assignment" 
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator "
        in
        if t1 = t2 then
          let t = match op with
              Add | Subtract | Multiply | Divide | Power when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less | Greater | GreaterEqual | LessEqual when t1 = Int -> Bool
            | And | Or -> Bool
            | _ -> raise (Failure "Fatal error.")
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure "Fatal error.") 
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure "fatal error")
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found "
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ))
    in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl

    and check_stmt =function
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If (predicate, then_stmts, else_stmts) -> SIf(check_bool_expr predicate, check_stmt_list then_stmts, check_stmt_list else_stmts)
      | While (predicate, loop_body) -> SWhile(check_bool_expr predicate, check_stmt_list loop_body)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives error" ))
    in 
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  
  
  

  in
  (List.map check_struct structs, (globals, List.map check_func functions))
