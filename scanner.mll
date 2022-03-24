{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule tokenize = parse
    [' ' '\t' '\r' '\n'] { tokenize lexbuf }
    | "true" { BOOL_LITERAL(true) }
    | "false" { BOOL_LITERAL(false) }
    | digit+ as value { INT_LITERAL(int_of_string value) }
    | eof { EOF }