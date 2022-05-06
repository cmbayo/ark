module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(*Translate: change the sast program to an LLVM module*)
let translate (globals,functions) = 
  let context = L.global_context () in
  let ark_module = L.create_module context "Ark" in

  let i32_t       = L.i32_type context
  and i8_t        = L.i8_type     context
  and i1_t        = L.i1_type context in

  let ltype_of_typ = function
    A.Int -> i32_t
    | A.Bool -> i1_t
    in
  
  let global_vars : L.llvalue StringMap.t =
      let global_var m (t, n) =
        let init = L.const_int (ltype_of_typ t) 0
        in StringMap.add n (L.define_global n init ark_module) m in
      List.fold_left global_var StringMap.empty globals in
  
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t ark_module in
  
  let function_decls : (L.llvalue * sfunc_dec) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and parameter_types =
          Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sparameters)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) parameter_types in
      StringMap.add name (L.define_function name ftype ark_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


    let build_function_body fdecl = 
      let (the_function, _) = StringMap.find fdecl.sfname function_decls in
      let builder = L.builder_at_end context (L.entry_block the_function) in
      
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

      let local_vars =
        let add_parameter m (t, n) p =
          L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          StringMap.add n local m
      
        and add_local m (t, n) =
          let local_var = L.build_alloca (ltype_of_typ t) n builder
          in StringMap.add n local_var m
        in
    
        let parameters = List.fold_left2 add_parameter StringMap.empty fdecl.sparameters
          (Array.to_list (L.params the_function)) in
        List.fold_left add_local parameters fdecl.slocals
      in

      let lookup n = try StringMap.find n local_vars
        with Not_found -> StringMap.find n global_vars
      in

      let rec build_expr builder ((_, e) : sexpr) = match e with
          SIntLiteral i -> L.const_int i32_t i
        | SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
        | SId s       -> L.build_load (lookup s) s builder
        | SAssign (s, e) -> let e' = build_expr builder e in 
          ignore(L.build_store e' (lookup s) builder); e'
        | SBinop (e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
          (match op with
             A.Add         -> L.build_add
           | A.Subtract    -> L.build_sub
           | A.And         -> L.build_and
           | A.Multiply    -> L.build_mul
           | A.Divide      -> L.build_udiv
           | A.Or      -> L.build_or
           | A.Equal   -> L.build_icmp L.Icmp.Eq
           | A.Neq     -> L.build_icmp L.Icmp.Ne
           | A.Less    -> L.build_icmp L.Icmp.Slt
          ) e1' e2' "tmp" builder
        | SCall ("print", [e]) ->
          L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
            "printf" builder
        | SCall (f, args) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
          let result = f ^ "_result" in
          L.build_call fdef (Array.of_list llargs) result builder
      in

      let add_terminal builder instr =
          match L.block_terminator (L.insertion_block builder) with
            Some _ -> ()
          | None -> ignore (instr builder) in

      let rec build_stmt builder = function
          SBlock sl -> List.fold_left build_stmt builder sl
        | SExpr e -> ignore(build_expr builder e); builder
        | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
        | SIf (predicate, then_stmt, else_stmt) ->
          let bool_val = build_expr builder predicate in

          let then_bb = L.append_block context "then" the_function in
          ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
          let else_bb = L.append_block context "else" the_function in
          ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

          let end_bb = L.append_block context "if_end" the_function in
          let build_br_end = L.build_br end_bb in (* partial function *)
          add_terminal (L.builder_at_end context then_bb) build_br_end;
          add_terminal (L.builder_at_end context else_bb) build_br_end;

          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context end_bb

        | SWhile (predicate, body) ->
            let while_bb = L.append_block context "while" the_function in
            let build_br_while = L.build_br while_bb in (* partial function *)
            ignore (build_br_while builder);
            let while_builder = L.builder_at_end context while_bb in
            let bool_val = build_expr while_builder predicate in

            let body_bb = L.append_block context "while_body" the_function in
            add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

            let end_bb = L.append_block context "while_end" the_function in
            ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
            L.builder_at_end context end_bb

        in
        let func_builder = build_stmt builder (SBlock fdecl.sbody) in

        add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in

  List.iter build_function_body functions;
  ark_module