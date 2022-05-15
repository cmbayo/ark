let () =
  let speclist = [] in
  let usage_msg = "usage: ./ark.native [-a|-s|-l] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let read_input chan =
    let safe_read_line chan =
      try Some(input_line chan) with
      End_of_file -> None
    in

    let rec read_input_helper input chan =
      match safe_read_line chan with
      | Some(line) -> read_input_helper (input ^ "\n" ^ line) chan
      | None -> input
    in
    read_input_helper "" chan
  in

  let source = read_input !channel in
  let lexbuf = Lexing.from_string source in
  let ast = Parser.program Scanner.tokenize lexbuf in
  let sast = Semantics.check ast in
  let codegen = Irgen.translate sast in
  print_string (Llvm.string_of_llmodule codegen)