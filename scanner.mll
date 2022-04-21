{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule tokenize = parse
    [' ' '\t' '\r' '\n'] { tokenize lexbuf }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | "**" { POWER }
    | "int" { INT }
    | "bool" { BOOL }
    | "=" { EQUAL }
    | "true" { BOOL_LITERAL(true) }
    | "false" { BOOL_LITERAL(false) }
    | "print" { PRINT }
    | digit+ as value { INT_LITERAL(int_of_string value) }
    | letter (digit | letter | '_')* as lem { ID(lem) }
    | '.' { PERIOD }
    | eof { EOF }