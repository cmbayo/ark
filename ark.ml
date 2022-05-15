type action = Ast | Sast | LLVM_IR

let safe_read_line() =
  try Some(read_line()) with
  End_of_file -> None

let read_input =
  let rec read_input_helper input =
    match safe_read_line() with
    | Some(line) -> read_input_helper (input ^ "\n" ^ line)
    | None -> input
  in
  read_input_helper ""

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
  ] in
  let usage_msg = "usage: ./ark.native [-a|-s|-l] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.tokenize lexbuf in
  match !action with
    Ast -> print_string "ast"
  | _ -> let sast = Semantics.check ast in
    match !action with
      Ast     -> ()
    | Sast    -> print_string "sast"
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))