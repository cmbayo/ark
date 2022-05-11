{ open Parser 

(* Scanf documentation: https://v2.ocaml.org/api/Scanf.html
   String implementation inspired by https://github.com/eureyuri/JQER *)
let str_scanner s =
         Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}
        
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
(* string can be any valid keyboard character *)
let string = '"' ( ( digit | letter | [' '-'/' ':'-'@' '['-'`' '{'-'~' 't' 'r' 'n' ''' '"' '\\'])* as s) '"'

rule tokenize = parse
    [' ' '\t' '\r' '\n'] { tokenize lexbuf }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }
    | "**" { POWER }
    | "int" { INT }
    | "bool" { BOOL }
    | "str" { STRING }
    | "==" { EQUAL }
    | "true" { BOOL_LITERAL(true) }
    | "false" { BOOL_LITERAL(false) }
    | "if" { IF }
    | "else" { ELSE }
    | "while" { WHILE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | '=' {ASSIGN}
    | digit+ as value { INT_LITERAL(int_of_string value) }
    | letter (digit | letter | '_')* as lem { ID(lem) }
    | string  { STRING_LITERAL( (str_scanner s) ) }
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
