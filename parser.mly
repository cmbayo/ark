%{ open Ast %}

%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token EOF

%start program
%type <Ast.program> program

%%

program:
    literal EOF { $1 }

literal:
    INT_LITERAL { IntLiteral $1 }
    | BOOL_LITERAL { BoolLiteral $1 }