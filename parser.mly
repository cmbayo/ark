%{
    open Ast
%}

%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token EOF

%start program_rule
%type <Ast.program> program_rule

%%

program_rule:
    literal_rule EOF { $1 }

literal_rule:
    INT_LITERAL { IntLiteral $1 }
    | BOOL_LITERAL { BoolLiteral $1 }