module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate(sprogram: Sast.sprogram) = 
  let context = L.global_context () in
  let ark_module = L.create_module context "Ark" in

  let i32_t = L.i32_type context
  and i1_t = L.i1_type context in

  let ltype_of_typ = function
    A.Int -> i32_t
    | A.Bool -> i1_t
    in
  
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in

  let builder = L.builder_at_end context (L.entry_block "test") in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

  let rec build_expr (builder, sexpr: Sast.sexpr) =
    match sexpr with
    SIntLiteral i -> L.const_int i32_t i1_t
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
  in

  let rec build_stmt (builder, sstmt: Sast.sstmt) =
    match sstmt with
    SExpr(sexpr) -> None (* TODO: This is only temporary *)
    | SPrint(sexpr) ->  ignore(
      L.build_call printf_func  [| int_format_str ; (build_expr builder sexpr) |]
      ); builder
  in

  build_stmt builder sprogram;
  ark_module

  