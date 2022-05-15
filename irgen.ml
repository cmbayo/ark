module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (structs, (globals, functions)) =
  let context    = L.global_context () in

  let ark_module = L.create_module context "ark" in

  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and string_t   = L.pointer_type (L.i8_type context) 
  and struct_t n  = L.named_struct_type context n in  
  
  let rec ltype_of_struct_variables = function
      A.Struct n -> struct_t n 
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.String -> string_t  
  in

  let structs_decls = 
    let struct_decl map sdecl =
      let name = sdecl.ssname
      and variable_types = Array.of_list (List.map (fun (t,_) -> ltype_of_struct_variables t) sdecl.ssvariables) in 
      let stype = L.struct_type context variable_types in
      StringMap.add name (stype, sdecl.ssvariables) map in
    List.fold_left struct_decl StringMap.empty structs
  in

  let lookup_struc s = 
  try StringMap.find s structs_decls
    with Not_found -> raise (Failure ("struct not found")) in
  
  let rec ltype_of_typ = function
    A.Int   -> i32_t
  | A.Bool  -> i1_t
  | A.String -> string_t
  | A.Struct n -> fst (lookup_struc n)
in

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
                A.String -> L.const_null (ltype_of_typ t)
                |A.Struct n -> L.const_null (ltype_of_typ t)
                | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init ark_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* let structs_vars : L.llvalue StringMap.t =
    let structs_var m s = 
      let init t = match t with 
                A.String -> L.const_null (ltype_of_typ t)
                | _ -> L.const_int (ltype_of_typ t) 0
      in List.fold_left
          (fun m (t, n) -> StringMap.add (s.ssname ^ ":" ^ n) (L.define_global (s.ssname ^ ":" ^ n) (init t) ark_module) m)
          m
          s.ssvariables in
    List.fold_left structs_var StringMap.empty structs in *)

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t ark_module in

  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype ark_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    let lookup map n = match StringMap.find_opt n map with
        Some (v, _) -> v 
      | None -> try StringMap.find n local_vars
                with Not_found -> StringMap.find n global_vars
    in

    let rec build_expr map builder ((_, e) : sexpr) = match e with
        SIntLiteral i  -> L.const_int i32_t i, map, builder
      | SBoolLiteral b  -> L.const_int i1_t (if b then 1 else 0), map, builder
      | SStringLiteral s -> L.build_global_stringptr s "str" builder, map, builder
      | SId s       -> L.build_load (lookup map s) s builder, map, builder   
      | SAssign (v, e) -> 
        let (e1, map1, builder) = build_expr map builder e in 
        ignore(L.build_store e1 (lookup map v) builder); e1, map1, builder

      | SStructAssign (v, m, e) ->
        let rval, map1, builder = build_expr map builder e in
        let a_addr = lookup map1 v in
        let strcut_name = (match snd (StringMap.find v map1) with A.Struct i -> i) in
        (* let _ = print_string strcut_name in *)

        let members = snd (lookup_struc strcut_name) in  
          let rec get_idx n lst i = match lst with
            | [] -> raise (Failure( "Struct member Error"))
            | hd::tl -> if (hd=n) then i else get_idx n tl (i+1)   
          in let idx = (get_idx m (List.map (fun (_,nm) -> nm) members) 0) in            
        let ptr = L.build_struct_gep a_addr idx ("struct_p") builder in
        let _ = L.build_store rval ptr builder in
        (rval, map1, builder)
      
      | SStructGet (v, m) -> 
        let name = match snd v with
            SId s -> s
        in
        let a_addr = lookup map name in
        let strcut_name = (match snd (StringMap.find name map) with A.Struct i -> i) in

        let mname = (match m with A.Id i -> i) in

        let members = snd (lookup_struc strcut_name) in  
          let rec get_idx n lst i = match lst with
            | [] -> raise (Failure( "Struct member Error"))
            | hd::tl -> if (hd=n) then i else get_idx n tl (i+1)   
          in let idx = (get_idx mname (List.map (fun (_,nn) -> nn) members) 0) in 
        let ptr = L.build_struct_gep a_addr idx ("struct_p") builder in
        let value = L.build_load ptr "member_v" builder in
        (value, map, builder)

      | SCall ("print", [e]) ->
        let e', _, builder = build_expr map builder e in L.build_call printf_func [| int_format_str ; e' |] "printf" builder, map, builder

      | SCall ("printstring", [e]) ->
        let e', _, builder = build_expr map builder e in L.build_call printf_func [| string_format_str ; e' |] "printf" builder, map, builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.map (fun(a,b,c) -> a) (List.rev (List.map (build_expr map builder) (List.rev args))) in 
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder, map, builder
      | SBinop (e1, op, e2) -> 
        let (e1', _, _) = build_expr map builder e1
        and (e2', _, _) = build_expr map builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Subtract -> L.build_sub
         | A.Multiply -> L.build_mul
         | A.Divide -> L.build_sdiv
         | A.Power -> L.build_mul (* TODO: THIS IS A PLACEHOLDER *)
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Greater    -> L.build_icmp L.Icmp.Sgt
         | A.GreaterEqual    -> L.build_icmp L.Icmp.Sge
         | A.LessEqual -> L.build_icmp L.Icmp.Sle
         | A.And -> L.build_and
         | A.Or -> L.build_or

        ) e1' e2' "tmp" builder, map, builder
        
    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    let rec build_stmt map builder = function
        SBlock sl -> let b, _ = List.fold_left (fun (b, m) s -> build_stmt m b s) (builder, map) sl in (b, map)
      | SExpr e -> ignore(build_expr map builder e); builder, map
      | SReturn e -> 
        let e',_,_ = build_expr map builder e in 
        ignore(L.build_ret e' builder); builder,map

      | SIf(e, s1, s2) -> 
        let bool_val, m', builder = build_expr map builder e in
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)
        let then_bb = L.append_block context "then" the_function in
        let then_builder, m'' = build_stmt m' (L.builder_at_end context then_bb) s1 in
          add_terminal then_builder build_br_merge;
        let else_bb = L.append_block context "else" the_function in
        let else_builder, m'' = build_stmt m' (L.builder_at_end context else_bb) s2 in
          add_terminal else_builder build_br_merge;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb, m'
    
      | SWhile(condition, stmtList) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore(L.build_br pred_bb builder);
        let body_bb = L.append_block context "while_body" the_function in
        let body_bldr, m' = build_stmt map (L.builder_at_end context body_bb) stmtList in
        add_terminal body_bldr (L.build_br pred_bb);
        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val, _, _ = build_expr m' pred_builder condition in
        let merge_bb = L.append_block context "merge" the_function in
        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb, m'


    in

    let func_builder, _  = build_stmt StringMap.empty builder (SBlock fdecl.sbody) in

    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  ark_module


(* module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate(globals, functions) = 
    let context = L.global_context () in

    let the_module = L.create_module context "Ark" in

    let i32_t = L.i32_type context 
    and i8_t = L.i8_type context 
    and i1_t = L.i1_type context
    in

    let ltype_of_typ = function
        A.Int -> i32_t
      | A.Bool -> i1_t
    in

    let global_vars : L.llvalue StringMap.t = 
        let global_var m (t,n) = 
            let init = L.const_int
            (ltype_of_typ t) 0
            in StringMap.add n (L.define_global n init the_module) m in
        List.fold_left global_var StringMap.empty globals in
    
        let printf_t : L.lltype =
            L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
        let printf_func : L.llvalue =
            L.declare_function "printf" printf_t the_module in

    
    let rec build_expr builder((_,e): sexpr) = match e with
        SIntLiteral i -> L.const_int i32_t i
        | SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
        | SId s -> L.build_load (lookup s) s builder
        | SAssign (s,e) -> let e' = build_expr builder e in 
          ignore(L.build_store e' (lookup s) builder); e'
        | SBinop (e1, op, e2) -> 
            let e1' = build_expr builder e1 
            and e2' = build_expr builder e2 in 
            (match op with
            A.Add       -> L.b uild_add
            | A.Subtract  -> L.build_sub
            | A.Multiply  -> L.build_mul
            | A.Divide    -> L.build_sdiv
            ) e1' e2' "tmp" builder
        in 

        List.iter build_function_body functions; 
        the_module *)
