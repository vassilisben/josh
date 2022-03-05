{ open Parse
  open Scanf }

let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = (alpha | '_') (alpha | digit | '_')* (* keywords??? *)
let string = '"' ( (ascii | escape)* as s ) '"'
let char = ''' ( ascii ) ''' (* digit? *)
let float = ((digit+) ['.'] digit*) | ((digit*) ['.'] digit+)
let int = digit+
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
  | whitespace { token lexbuf }
  | "{|"     { comment lexbuf }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ';'      { SEMI }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULT }
  | '/'      { DIV }
  | '='      { ASSIGN }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | '>'      { GT }
  | ">="     { GEQ }
  | "and"    { AND }
  | "or"     { OR }
  | "not"    { NOT }
  | "if"     { IF }
  | "else"   { ELSE }
  | "for"    { FOR }
  | "in"     { IN }
  | "while"  { WHILE }
  | "break"  { BREAK }
  | "continue" { CONTINUE }
  | "return" { RETURN }
  | "int"    { INT }
  | "bool"   { BOOL }
  | "float"  { FLOAT }
  | "string" { STRING }
  | "void"   { VOID }
  | "char"   { CHAR }
  | "list"   { LIST } 
  | "true"   { BOOLLIT(true)  }
  | "false"  { BOOLLIT(false) }
  | "type"   { TYPEDEF }
  | "record" { RECORD }
  | int as lem { INTLIT(int_of_string lem) }
  | float as lem { FLOATLIT(float_of_string lem)}
  | char as lem { CHARLIT(String.get lem 1)}
  | string as lem { STRLIT(Scanf.unescaped lem)}
  | escape_char as lem { CHARLIT(String.get (Scanf.unescaped lem) 1) }
  | id as lem { ID(lem) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    | "|}"  { token lexbuf }
    | _     { comment lexbuf }
