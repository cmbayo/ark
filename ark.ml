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
  let speclist = [] in
  let usage_msg = "usage: ./ark.native [-a|-s|-l] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.tokenize lexbuf in
  let sast = Semantics.check ast in
  let codegen = Irgen.translate sast in
  print_string (Llvm.string_of_llmodule codegen)