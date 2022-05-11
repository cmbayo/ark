%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE POWER
%token INT BOOL
%token EQUAL
%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID
%token IF ELSE
%token LBRACE RBRACE LPAREN RPAREN
%token ARROW COLON ELLIPSIS
%token PERIOD COMMA
%token RETURN DEF INPUT OUTPUT
%token EOF

%start program
%type <Ast.program> program

%left PLUS MINUS
%left TIMES POWER DIVIDE

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], []) }
 | vdecl PERIOD decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl PERIOD vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int }
  | BOOL  { Bool }

/* fdecl */
fdecl:
  ID LBRACE formals_opt RBRACE ARROW typ COLON vdecl_list stmt_list ELLIPSIS
  {
    {
      rtyp=$6;
      fname=$1;
      formals=$3;
      locals=$8;
      body=$9
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr PERIOD { Expr $1 }
  | IF LPAREN expr RPAREN COLON stmt_list ELLIPSIS ELSE COLON stmt_list ELLIPSIS { If($3, $6, $10) }
  | IF LPAREN expr RPAREN COLON stmt_list ELLIPSIS { If($3, $6, []) }
  | COLON stmt_list ELLIPSIS { Block $2 }

expr:
    INT_LITERAL { IntLiteral $1 }
  | BOOL_LITERAL { BoolLiteral $1 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Subtract,   $3)   }
  | expr TIMES expr { Binop ($1, Multiply, $3) }
  | expr DIVIDE expr { Binop ($1, Divide, $3) }
  | expr POWER expr { Binop ($1, Power, $3)}
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
