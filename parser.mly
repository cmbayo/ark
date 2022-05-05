%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE POWER
%token INT BOOL
%token EQUAL
%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID
%token PRINT
%token IF ELSE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PERIOD
%token RETURN COMMA
%token EOF
%token WHILE

%start program
%type <Ast.program> program

%left PLUS MINUS
%left TIMES POWER DIVIDE

%%

program:
    decls EOF { $1 }

decls:
    {([],[])} /* No global declaration, no function declarations*/
    |vdecl PERIOD decls {(($1 :: fst $3), snd $3)}
    |fdecl decls {(fst $2, ($1 :: snd $2))}
    /* Here we are able to declare global variables, and then functions, and then global variables.*/

/*Function declaration*/
/* int gcd (int a, int b)*/
/* function_name (input, output) {   }*/
fdecl: vdecl LPAREN parameters_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
{
    {
        rtyp = fst 1;
        fname = snd $1;
        parameters = $3;
        locals = $6;
        body = $7
    }
}

parameters_opt:
    {[]}
    | parameters {$1}


parameters:
    vdecl   {[$1]}
    | vdecl COMMA parameters {$1 :: $3}

vdecl_list:
  /*nothing*/ { [] }
  | vdecl PERIOD vdecl_list  {  $1 :: $3 }
  
/* int x*/
vdecl:
    typ ID {($1, $2)}
stmt:
    PRINT expr PERIOD { Print $2 }
    | expr PERIOD { Expr $1 }
    | LBRACE stmt RBRACE { Block $2 }
    | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
    | WHILE LPAREN expr RPAREN stmt {While ($3, $5)}
    | RETURN expr PERIOD    {Return $2}

expr:
    expr PLUS expr { Binop ($1, Add, $3) }
    | expr MINUS expr { Binop ($1, Subtract, $3) }
    | expr TIMES expr { Binop ($1, Multiply, $3) }
    | expr DIVIDE expr { Binop ($1, Divide, $3) }
    | expr POWER expr { Binop ($1, Power, $3)}
    | literal { $1 }
    | typ ID EQUAL expr { Assign($1, $2, $4) }
    /* f(x,y+z)*/
    | ID LPAREN args_opt RPAREN { Call ($1, $3)}

    /* function_name (input, output) {   }*/
args_opt: 
        { [] }
    | args {$1} /*contains one or more arguments*/

args:
    expr {[$1]} /*one expression, no comma*/
    | expr COMMA args_opt {$1::$3} /*More than one arguments*/

typ:
    INT { Int } 
    | BOOL { Bool }

literal:
    INT_LITERAL { IntLiteral $1 }
    | BOOL_LITERAL { BoolLiteral $1 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }