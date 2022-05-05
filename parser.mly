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

%start program
%type <Ast.program> program

%left PLUS MINUS
%left TIMES POWER DIVIDE

%%

program:
    stmt EOF { $1 }

stmt:
    PRINT expr PERIOD { Print $2 }
    | expr PERIOD { Expr $1 }
    | LBRACE stmt RBRACE { Block $2 }
    | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }

expr:
    expr PLUS expr { Binop ($1, Add, $3) }
    | expr MINUS expr { Binop ($1, Subtract, $3) }
    | expr TIMES expr { Binop ($1, Multiply, $3) }
    | expr DIVIDE expr { Binop ($1, Divide, $3) }
    | expr POWER expr { Binop ($1, Power, $3)}
    | literal { $1 }
    | typ ID EQUAL expr { Assign($1, $2, $4) }

typ:
    INT { Int } | 
    BOOL { Bool }

literal:
    INT_LITERAL { IntLiteral $1 }
    | BOOL_LITERAL { BoolLiteral $1 }