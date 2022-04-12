type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COMMA
  | DOT
  | PLUS
  | MINUS
  | ASSIGN
  | MULT
  | DIV
  | MOD
  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | AND
  | OR
  | NOT
  | IF
  | ELSE
  | FOR
  | IN
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN
  | INT
  | BOOL
  | FLOAT
  | STRING
  | VOID
  | CHAR
  | RECORD
  | INTLIT of (int)
  | CHARLIT of (char)
  | STRLIT of (string)
  | BOOLLIT of (bool)
  | TRUE
  | FALSE
  | FLOATLIT of (float)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
