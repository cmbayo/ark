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
    | "==" { EQUAL }
    | "true" { BOOL_LITERAL(true) }
    | "false" { BOOL_LITERAL(false) }
    | "if" { IF }
    | "else" { ELSE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | '=' {ASSIGN}
    | digit+ as value { INT_LITERAL(int_of_string value) }
    | letter (digit | letter | '_')* as lem { ID(lem) }
    | "..." { ELLIPSIS }
    | '.' { PERIOD }
    | ',' { COMMA }
    | ':' { COLON }
    | "->" { ARROW }
    | "def" { DEF }
    | "input" { INPUT }
    | "output" { OUTPUT }
    | "return" { RETURN }
    | eof { EOF }
    | eof { EOF }