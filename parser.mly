/* Ocamlyacc parser for NanoC */

%{
open Ast
%}

%token BASH SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS ASSIGN MULT DIV
%token EQ NEQ LT GT LEQ GEQ AND OR NOT
%token IF ELSE FOR IN WHILE BREAK CONTINUE RETURN
%token INT BOOL FLOAT STRING VOID CHAR LIST
%token <int> INTLIT
%token <char> CHARLIT
%token <string> STRLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program:
  vdecl_list stmt_list EOF { {locals=$1; body=$2} }

vdecl_list:
  /*nothing*/                   { []       }
  | vdecl vdecl_list  { $1 :: $2 }

vdecl:
  typ ID SEMI { ($1, $2) }


typ:
  INT       { Int  }
  | BOOL    { Bool }
  | FLOAT   { Float }
  | CHAR    { Char }
  | STRING  { String }
  | LIST typ    { List($1) }
  | VOID    { Void }
  | RECORD ID { RecordType($2) }

stmt_list:
    /* nothing */               { [] }
    | stmt stmt_list  { $1::$2 }

stmt:
  expr SEMI                                          { Expr $1         }
  | LBRACE stmt_list RBRACE                          { Block $2        }
  | IF LPAREN expr RPAREN stmt ELSE stmt   { If ($3, $5, $7) }
  | FOR LPAREN ID IN expr RPAREN stmt { For ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt               { While ($3,$5)   }
  | RECORD ID LBRACE opts_list RBRACE { RecordDef($2, $4) }

expr_list:
    /* nothing */   { [] }
    | expr_list COMMA expr { $3::$1 }

expr:
  /* literals */
  | BOOLLIT                      { BoolLit $1  }
  | INTLIT                       { IntLit $1   }
  | FLOATLIT                     { FloatLit $1 }
  | CHARLIT                      { CharLit $1  }
  | STRLIT                       { StrLit $1   }
  | TRUE             { BoolLit(true)  }
  | FALSE            { BoolLit(false) }
  | ID                           { Id $1         }
  /* arithmetic expressions */
  | expr PLUS expr      { Binop ($1, Add, $3)   }
  | expr MINUS expr     { Binop ($1, Sub, $3)   }
  | expr DIV expr       { Binop ($1, Add, $3)   }
  | expr MULT expr      { Binop ($1, Sub, $3)   }
  | expr MOD expr        { Binop ($1, Mod, $3)  }
  /* equality */
  | expr EQ expr        { Binop ($1, Equal, $3) }
  | expr NEQ expr       { Binop ($1, Neq, $3)   }
  | expr LT expr        { Binop ($1, Less, $3)  }
  | expr LEQ expr       { Binop ($1, Leq, $3)  }
  | expr GT expr        { Binop ($1, Greater, $3)  }
  | expr GEQ expr       { Binop ($1, Geq, $3)  }
  | expr LT expr        { Binop ($1, Less, $3)  }
  /* logical */
  | expr AND expr       { Binop ($1, And, $3)   }
  | expr OR expr        { Binop ($1, Or, $3)    }
  | NOT expr        { Unop (Not, $2)    }
  | ID ASSIGN expr           { Assign ($1, $3)       }
  | LPAREN expr RPAREN       { $2                    }
  /* list */
  | LBRACK expr_list RBRACK { ListLit(List.rev $2)  }
  | expr LBRACK expr RBRACK { ListAccess($1, $2) }
  | expr DOT ID { RecordAccess($1, $2) }
  /* record instantiation */
  | ID LBRACE actuals_list RBRACE { RecordCreate($1,$3) }
  /* mutation */
  | expr DOT ID ASSIGN expr { MutateRecord(($1,$3), $4) }
  | expr LBRACK expr RBRACK ASSIGN expr { MutateList(($1,$3), $6) }

decl_var:
  | TYP ID      { Declare($1, $2) }
  | TYP ID ASSIGN expr  { Initialize($1, $2, $4)}

/* for record field and function argument lists */
opts_list:
    /* nothing */   { [] }
    | opts_list COMMA opt { $3::$1 }

opt:
    typ ID { Opt($1,$2) }

/* for instantiating records and calling functions */
actuals_list:
    /* nothing */ { [] }
    | actuals_list COMMA actual { $3::$1 }

actual:
    expr { $1 }
