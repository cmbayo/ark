%{ open Ast %}

%token LBRACE RBRACE LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE POWER ASSIGN
%token INT BOOL STRING
%token STRUCT
%token EQ NEQ LT GT LEQ GEQ AND OR
%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID STRING_LITERAL
%token <string> STRUCT_ID
%token IF ELSE WHILE
%token ARROW COLON ELLIPSIS
%token PERIOD COMMA
%token RETURN DEF INPUT OUTPUT
%token EOF

%right ASSIGN
%left PLUS MINUS
%left TIMES POWER DIVIDE
%left LBRACE RBRACE
%left LPAREN RPAREN

%start program
%type <Ast.program> program
%%

/* add function declarations*/
program:
  body EOF { $1}

body:
  structdecl_list decls   {($1, $2)}

structdecl_list:
  /* nothing */		              { []      }
  | structdecl structdecl_list    { $1::$2  }

structdecl:
  STRUCT STRUCT_ID LBRACE vdecl_list RBRACE PERIOD {
                {
                  sname = $2;
                  svariables = $4;
                }
  }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl PERIOD vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }
  

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
  | IF LPAREN expr RPAREN COLON stmt ELLIPSIS ELSE COLON stmt ELLIPSIS { If($3, $6, $10) }
  | IF LPAREN expr RPAREN COLON stmt ELLIPSIS { If($3, $6, Block([])) }
  | WHILE LPAREN expr RPAREN COLON stmt ELLIPSIS { While($3, $6) }
  | COLON stmt_list ELLIPSIS { Block $2 }
  | RETURN expr PERIOD {Return $2}

expr:
    INT_LITERAL { IntLiteral $1 }
  | BOOL_LITERAL { BoolLiteral $1 }
  | STRING_LITERAL { StringLiteral $1 }
  | ID            {Id ($1)}
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Subtract,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr TIMES expr { Binop ($1, Multiply, $3) }
  | expr DIVIDE expr { Binop ($1, Divide, $3) }
  | expr POWER expr { Binop ($1, Power, $3)}
  | expr AND expr       { Binop ($1, And, $3)   }
  | expr OR expr        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr   { Assign($1, $3) }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  /*Struct assign and get*/
  | ID ARROW ID             { StructGet(Id($1), Id($3))       }
  | ID COLON ID ASSIGN expr { StructAssign($1,$3,$5)  }
  

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }

decls:
   /* nothing */ { ([], []) }
 | vdecl PERIOD decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }



typ:
  INT   { Int }
  | BOOL  { Bool }
  | STRING { String }
  | STRUCT_ID {Struct($1)}
