module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(*Translate: change the sast program to an LLVM module*)
let translate(sprogram: Sast.sprogram) = 
  let context = L.global_context () in
  let ark_module = L.create_module context "Ark" in

  let i32_t = L.i32_type context
  and i8_t       = L.i8_type     context
  and i1_t = L.i1_type context in

  let ltype_of_typ = function
    A.Int -> i32_t
    | A.Bool -> i1_t
    in
  
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t ark_module in

      let builder = L.builder_at_end context (L.entry_block printf_func) in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

  let build_expr builder ((_, e) : sexpr) =
    match e with
    SIntLiteral i -> L.const_int i32_t i
    | SBoolLiteral b -> L.const_int i1_t (if b then 1 else 0)
  in
  let build_stmt builder  = function
    SExpr e -> ignore(build_expr builder e); builder (* TODO: This is only temporary *)
    | SPrint(e) ->  ignore(
      L.build_call printf_func  [| int_format_str ; (build_expr builder e) |] "printf" builder
      ); builder
  in

  build_stmt builder sprogram;
  ark_module