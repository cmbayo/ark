%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token EOF

%start program
%type <Ast.program> program

%left PLUS MINUS
%left TIMES DIVIDE

%%

program:
    expr EOF { $1 }

expr:
    expr PLUS expr { Binop ($1, Add, $3) }
    | expr MINUS expr { Binop ($1, Subtract, $3) }
    | expr TIMES expr { Binop ($1, Multiply, $3) }
    | expr DIVIDE expr { Binop ($1, Divide, $3) }
    | literal { $1 }

literal:
    INT_LITERAL { IntLiteral $1 }
    | BOOL_LITERAL { BoolLiteral $1 }