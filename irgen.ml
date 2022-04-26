module L = Llvm
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
            A.Add       -> L.build_add
            | A.Subtract  -> L.build_sub
            | A.Multiply  -> L.build_mul
            | A.Divide    -> L.build_sdiv
            ) e1' e2' "tmp" builder
        in 

        List.iter build_function_body functions; 
        the_module
