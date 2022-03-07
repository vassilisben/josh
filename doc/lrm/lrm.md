---
title: "Josh Reference Manual"
author:
- Bora Elci
- Burcu Cetin
- Angel Garcia
- Vasilis Benopoulos
- Gregory Schare
indent: false
mainfont: Georgia
monofont: Cascadia Mono Regular ExtraLight
monofontoptions:
  - Scale=0.9
numbersections: true
fontsize: 12pt
output:
    pdf_document:
        latex_engine: xelatex
header-includes:
  - \usepackage{fontspec}
  - \usepackage{indentfirst}
---

# Introduction
Josh is your friendly neighborhood shell scripting language. It is consistent
with UNIX commands allowing to include bash scripts and enhanced with
Python-like syntax to facilitate the learning of scripting and automation. Josh
uses error and type checking prior to running scripts in order to allow for
safety and avoid cluttering state if a part of the script is not valid. Unlike
Perl and Bash, Josh is less complex and built for readability and reusability
through intuitive syntax and statically scoped. Josh also differs from Python
as it is more minimalistic and less generalized/abstract. Josh also aims to cut
down running into runtime errors as it is a static, strongly typed language.

Josh is not intended for general-purpose use or for complex systems. Josh
focuses on education, and it is designed for beginner to intermediate level
programmers to familiarize themselves with bash, scripting languages, and UNIX
command-line through readable code with easier syntax while minimizing error
and expediting automation for routine tasks and testing.


# Lexical Analysis and Conventions
## Tokens
Josh has 5 types of tokens: identifiers, keywords, literals, operators, and
special symbols (such as separators).

Whitespace (spaces, tabs, newlines) are ignored and have no meaning, except
when they separate tokens.

Regular expression:
```
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
  | whitespace { token lexbuf }
```

## Comments
Josh supports single line and block comments which start with the character
sequence `{|` and end with `|}`. Comments are discarded by the scanner.

Comments may not be nested.

Example:
```
{| This is a single line comment. |}
```
```
{| This is a multi-line comment.
 | We recommend formatting comments with pipes like this.
 | The pipes make it easy to read.
 | (and we think it looks quite nice!)
 |}
```

Scanner rule:
```
rule token = parse
  | "{|"     { comment lexbuf }

and comment = parse
    | "|}"  { token lexbuf }
    | _     { comment lexbuf }
```


## Identifiers
Identifiers are a sequence of letters, digits, and underscores where the first
character is a letter or an underscore. Identifiers cannot be keywords.

Example:  
`foo`, `foo123`, `a`, `prevNode`, `snake_case_var`, `MdbRec`

Regular expression:
```
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = (letter | '_') (letter | digit | '_')*
```

## Keywords
The identifiers below are keywords which are reserved and can only be used for
their specific purpose in the programming language.

----   ------   ------
and    while    string
or     break    void
not    continue char
if     return   true
else   int      false
for    bool     record
in     float
----   ------   ------

`int`, `float`, `bool`, `char`, `void`, and `string` are used to declare the
type of variables, fields of records, and arguments and return values of functions.

`true` and `false` are literal values for the `bool` type.

`and`, `or`, and `not` are logical operators.

`if`, `else`, `for`, `in`, `while`, `break`, `continue`, and `return` are
associated with control flow statements.

`record` initiates the declaration of a new record type.

## Literals
Josh supports literal values for five types: int, float, bool, char, and string.

For more details on how they are tokenized, see the Appendix section on the
scanner.

### int
Integer literals are a sequence of one or more digits in decimal, which may be
preceded by a minus sign indicating it is negative.

Example:  
`0`, `25`, `-1`, `12345`

Regular expression:
```
let digit = ['0'-'9']
let int = ('-')? digit+
```

### float
Float literals are one or more digits with a decimal point somewhere before,
after, or in the middle. Like integers, they may be negated.

Example:  
`0.0`, `.5`, `12.34`, `1.000001`, `25.`

Regular expression:
```
let digit = ['0'-'9']
let float = ('-')? ((digit+) ['.'] digit*) | ((digit*) ['.'] digit+)
```

### bool
Boolean literals have two values represented by the keywords `true` and `false`.

### char
Character literals are ASCII characters enclosed in single quotes. Escape
characters are also character literals starting with a backslash and followed
by the specific ASCII characters listed below.

Example:
`'a'`, `'5'`, `'\n'`, `'}'`, `' '`

Regular expression:
```
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let char = ''' ( ascii ) '''
```

### string
String literals are a sequence of zero or more characters enclosed in double
quotes.

Example:  
`"Hello, World!"`, `"abc123"`, `""`, `"    \n    "`, `"\\LaTeX"`

Regular expression:
```
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let string = '"' ( (ascii | escape)* as lem ) '"'
```

## Operators
The operators can be grouped into four categories: arithmetic, assignment,
comparison, and logical.

They are all scanned directly as strings or characters.

Regular expression:
```
rule token = parse
  (* Arithmetic *)
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULT }
  | '/'      { DIV }
  | '%'      { MOD }
  (* Assignment *)
  | '='      { ASSIGN }
  (* Comparison *)
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | '>'      { GT }
  | ">="     { GEQ }
  (* Logical *)
  | "and"    { AND }
  | "or"     { OR }
  | "not"    { NOT }
```

## Separators
The following special symbols or separators are specific tokens reserved for
separating other types of tokens.

- Commas delimit arguments and record fields.
- Semicolons delimit statements.
- Parentheses are used to group expressions and call functions.
- Braces are used to group blocks and construct records.
- Brackets are used to construct and index lists.
- Dots are used to access record fields and write literal floats.

Regular expression:
```
rule token = parse
  | ','      { COMMA }
  | ';'      { SEMI }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | '.'      { DOT }
```

# Types
Josh is strongly typed. Every expression has a type that is determined at
compile time. Every variable must be declared with a type. When a function or
operation is called with the wrong types, it will result in a type error.

Types can be split into two categories: primitive and non-primitive.
## Primitive Data Types
The primitive data types are, as you might expect, the same as those which
have literal values described above. They are int, float, bool, char, and
string. There is also void.

### int
The int type stores integer values.

Example:
```
int age = 21;
```

Grammar:
```
typ:
  INT       { Int  }
```

### float
The float type stores decimal values up to a certain precision.

Example:
```
float pi = 3.14159;
```

Grammar:
```
typ:
  FLOAT   { Float }
```

### bool
The bool type stores true and false values.

Example:
```
bool isEmpty = false;
```

Grammar:
```
typ:
  BOOL    { Bool }
```

### char
The char type stores single character values.

Example:
```
char letter = 'a';
```

Grammar:
```
typ:
  CHAR    { Char }
```

### string
The string type stores a sequence of characters.

Example:
```
string name = "Josh";
```

Grammar:
```
typ:
  STRING  { String }
```

### void
The void type does not store any value. It is used for functions that do not
return anything.

Example:
```
void printA(string a) { echo(a); }
```

Grammar:
```
typ:
  VOID    { Void }
```

## Non-Primitive Data Types
### Lists
Josh also supports dynamic arrays, which we call lists. Every element of a list
must be the same type. Lists can be nested.

Example:
```
[int] myList = [1, 2, 3];
[[char]] dnaPairs = [['a', 't'], ['c', 'g']];
```

Grammar:
```
typ:
  LBRACK typ RBRACK { ListT($2) }
```

### Records
Records are similar to structs in C.
A record creates a custom data type that can be used to associate a finite
number of items of different types.

Example:
```
record Person = {
    string name,
    int age
}
Person josh = Person { "Josh", 21 };
```

Grammar:
```
typ:
  ID { RecordType($1) }
```

### Functions
Functions can be types in Josh. This allows the programmer to include functions
inside of lists and records in order to create powerful abstractions and
generic algorithms.

Example:
```
record Node {
    int data;
    int cmp(Node n1, Node n2);
}
```

Grammar:
```
typ:
  typ ID LPAREN opts_list RPAREN { FuncType($2, $4, $1) }
```

## Type conversions
Some types may be, in some cases, sensibly converted into another. All type
conversions are explicit. See Standard Library.

# Program
A Josh program consists of a series of statements and function declarations.

Grammar:
```
program:
  top_level_list EOF { $1 }

top_level_list:
  { [] }
  | top_level top_level_list { $1::$2 }

top_level:
    stmt { Stmt $1 }
  | fdecl { Fdecl $1 }
```

# Statements
Statements are where the action happens. Whereas function declarations define
how data can be transformed, statements define what kind of data there can be
and actually perform these transformations. Most statements can be thought of
as the "verbs" of your program.

## Expressions
The simplest kind of statement is an expression followed by a semicolon. See
Expressions.

Example:
```
x = 5;
```

Grammar:
```
stmt:
  expr SEMI  { Expr $1 }
```

## Variable Declarations
Variable declarations count as statements when followed by a semicolon. See
Variable Declarations.

Example:
```
int x;
int y = 5;
```

Grammar:
```
stmt:
  vdecl SEMI   { Vdecl $1 }
```

## Blocks
Series of statements can be grouped into blocks using braces. They are read
and executed sequentially.

Example:
```
{
    int x = 5;
    echo("Hello, World!");
}
```

Grammar:
```
stmt_list:
    /* nothing */     { [] }
    | stmt stmt_list  { $1::$2 }

stmt:
  LBRACE stmt_list RBRACE   { Block $2 }
```

## Conditionals
Josh supports conditional operators identically to C.

The `if` keyword initiates a statement containing a condition, which is an
expression with a Boolean value, and another statement (most commonly a series
of statements surrounded by braces). Optionally, you can use the `else` keyword
after the statement following the `if` to begin a statement or series of
statements which are executed if the `if` condition is false.

We resolve the dangling else problem by using a `%prec` directive in our parser grammar.

Example:
```
if (condition1 and condition2) {
    int x = 5;
} else {
    int x = 10;
}
```

Grammar:
```
stmt:
  | IF LPAREN expr RPAREN stmt ELSE stmt   { If ($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt             { If ($3, $5, Expr(Noexpr)) }
```

## While loops
While loops are identical to C. A while loop is used to repeat a statement or
list of statements until a condition evaluates to false.

Example:
```
{| FizzBuzz in Josh. |}
int x = 0;
while (x < 100) {
    string s = “”;
    if (x % 5 == 0) {
        s = s + “Fizz”;
    }
if (x % 3 == 0) {
        s = s + “Buzz”;
    }
    if (s.length == 0) {
        echo(int_to_string(x));
    } else {
        echo(s);
    }
    x = x + 1;
}
```

Grammar:
```
stmt:
  WHILE LPAREN expr RPAREN stmt  { While ($3,$5) }
```

## For loops
For loops work similarly to Python’s for-in loops. A for statement loops over
the elements of a list and, for each element, executes a statement or list of
statements in the body of the loop with the element in scope.

Example:
```
for (x in [1, 2, 3]) {
    int y = x;
    x = x * 2;
    echo(int_to_string(x + y));
}
```

Grammar:
```
stmt:
  FOR LPAREN ID IN expr RPAREN stmt { For ($3, $5, $7) }
```

## Break
Break works like in C and Python. If inside a loop, the `break` keyword exits
the most immediate loop even if the iteration of a for loop has not finished or
the condition of a while loop is still true.

Example:
```
for (x in myList) {
    if (x == y) {
        break;
    }
    y = y + x;
}
```

Grammar:
```
stmt:
  | BREAK SEMI    { Break }
```

## Continue
Continue is like `break`, except instead of exiting the loop completely, it
simply goes back to the top of the loop without executing the rest of the
statements in the body of the loop. In a for loop, this does mean that it
proceeds to the next element.

Example:
```
while (p) {
    if (q) {
        continue;
    }
    total = total + 1;
}
```

Grammar:
```
stmt:
  | CONTINUE SEMI { Continue }
```

## Records
The definition of a record is a statement. Recall the example from the section
on non-primitive types.

Example:
```
record Person = {
    string name,
    int age
}
```

Grammar:
```
stmt:
  RECORD ID LBRACE opts_list RBRACE SEMI { RecordDef($2, $4) }

/* for record field and function argument lists */
opts:
  typ ID  { [Opt($1,$2)] }
  | opts COMMA typ ID { Opt($3,$4) :: $1 }

opts_list:
  /* nothing */ { [] }
  | opts { List.rev $1 }
```

## Return statements
Functions may return the value of a single expression whose type is consistent
with the return type of the function. For a function returning `void`, it is
acceptable to write `return;` with no expression.
If a return statement is executed, the function exits
and is popped from the call stack.

Example:
```
int add(int a, int b) {
    return a + b;
}

void print(s) {
    echo(s);
    return;
}
```

Grammar:
```
stmt:
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
```

# Expressions
# Parenthesized expressions
Expressions can be parenthesized to specify associativity.

Example:
`(a + 5)`

Grammar:
```
expr:
  LPAREN expr RPAREN  { $2 }
```

## Literals
Literal values of primitive types are expressions. See section on Literals.

Example:  
`false`, `5`, `2.5`, `'a'`, `"hello"`

Grammar:
```
expr:
   BOOLLIT     { BoolLit $1  }
  | INTLIT     { IntLit $1   }
  | FLOATLIT   { FloatLit $1 }
  | CHARLIT    { CharLit $1  }
  | STRLIT     { StrLit $1   }
  | TRUE       { BoolLit(true)  }
  | FALSE      { BoolLit(false) }
```

## Arithmetic operator expressions
The arithmetic operators are used as binary operators of two sub-expressions.
They generally return an integer or float depending on context.

Example:  
`5 + a`

Grammar:
```
expr:
  | expr PLUS expr      { Binop ($1, Add, $3)   }
  | expr MINUS expr     { Binop ($1, Sub, $3)   }
  | expr DIV expr       { Binop ($1, Div, $3)   }
  | expr MULT expr      { Binop ($1, Mul, $3)   }
  | expr MOD expr       { Binop ($1, Mod, $3)  }
```

## Comparisons
The comparison operators are used as binary operators of two sub-expressions.
They always return a Boolean.

Example:  
`5 > 6`, `x == y`

Grammar:
```
expr:
  | expr EQ expr        { Binop ($1, Equal, $3) }
  | expr NEQ expr       { Binop ($1, Neq, $3)   }
  | expr LT expr        { Binop ($1, Less, $3)  }
  | expr LEQ expr       { Binop ($1, Leq, $3)  }
  | expr GT expr        { Binop ($1, Greater, $3)  }
  | expr GEQ expr       { Binop ($1, Geq, $3)  }
```

## Logical expressions
The logical operators are used as binary operators of two Boolean sub-expressions.
They always return a Boolean.

Example:  
`true and false`

Grammar:
```
expr:
  | expr AND expr       { Binop ($1, And, $3)   }
  | expr OR expr        { Binop ($1, Or, $3)    }
  | NOT expr        { Unop (Not, $2)    }
```

## Assignment
The assignment of a variable (given by its identifier) to the value of an
expression is itself an expression. These associate to the right.

Assignment of an expression to a record field or list element is parsed
differently (see below).

Example:
```
x = 5;
a = b = c = 10;   {| same as `a = (b = (c = 10))` |}
```

Grammar:
```
expr:
  ID ASSIGN expr   { Assign ($1, $3) }
```

## List expressions
### Creation
Lists are not quite literals, but the creation of a list is an expression which
can be assigned to a variable with list type.

Example:
`[1, 2, 3]`, `[]`, `["a", "b", "c"]`

Grammar:
```
expr:
  LBRACK expr_list RBRACK { ListLit(List.rev $2)  }
expr_list:
  /* nothing */   { [] }
  | expr_list COMMA expr { $3::$1 }
```

### Access
List elements can also be accessed by index.

Example:
```
myList[0]
```

Grammar:
```
expr:
  expr LBRACK expr RBRACK { ListAccess($1, $3) }
```

### Mutation
List elements can be modified using similar syntax to assignment.

Example:
```
myList[0] = 'a'
```

Grammar:
```
expr:
  expr LBRACK expr RBRACK ASSIGN expr { MutateList(($1,$3), $6) }
```

## Record expressions
### Creation
Like lists, records can be instantiated and assigned to a variable or used like
literals.

Example:
```
Person{"Josh", 21}
```

Grammar:
```
expr:
  ID LBRACE actuals_list RBRACE { RecordCreate($1, $3) }

/* for instantiating records and calling functions */
actuals_list:
    /* nothing */ { [] }
    | actuals_list COMMA actual { $3::$1 }

actual:
    expr { Actual $1 }
```

### Access
Record fields can also be accessed using the identifier for that field.

Example:
```
josh.name
```

Grammar:
```
expr:
  expr DOT ID { RecordAccess($1, $3) }
```

### Mutation
Record fields can be modified using similar syntax to assignment.

Example:
```
dread_pirate_roberts.name = "Wesley"
```

Grammar:
```
expr:
  expr DOT ID ASSIGN expr { MutateRecord(($1,$3), $5) }
```

## Function calls
You can call a function using its identifier.

Example:
```
myFunc(arg1, arg2)
```

Grammar:
```
expr:
  ID LPAREN actuals_list RPAREN { Call($1, $3) }
```

You can call a function stored in a list.

Example:

```
listOfFunctions[3](arg1, arg2)
```

Grammar:
```
expr:
  expr LBRACK expr RBRACK LPAREN actuals_list RPAREN { CallList(($1,$3), $6) }
```

You can call a function stored in a record.

Example:  
```
node.cmp(other_node)
```

Grammar:
```
expr:
  expr DOT ID LPAREN actuals_list RPAREN { CallRecord(($1,$3), $5) }
```

# Declarations
## Variable declarations
Variables must be declared. Optionally, they can be initialized at the same
time they are declared. Thus there are two ways to declare a variable:

Example:
```
int x;
int y = 5;
```

Grammar:
```
vdecl:
    typ ID { Declare($1, $2) }
  | typ ID ASSIGN expr { Initialize($1, $2, $4)}
```

## Function declarations
Function declarations are identical to those in C.

Example:
```
int main(string args) {
    echo("Hello, World!");
    return 0;
}
```

Grammar:
```
fdecl:
    typ ID LPAREN opts_list RPAREN LBRACE stmt_list RBRACE
    { { id=$2; params=$4; body=$7; return_type=$1 } }
```


# Standard Library
Josh's functionality is extended through standard library functions.

Many features central to the language are explained here.

## Bash
Users are able to run bash scripts using the `bash` function, which takes as
an input the desired script as a string. The output of the command will be
returned as a string literal. The command itself will not be parsed. If the
user wants to use quotes as part of the command, they have to escape the char
with `\`

Example:
```
string ret = bash("man -f ls > out.txt | grep -n legacy")
```

Should the execution of the script fail, the Josh program will continue to run.

We will also implement common commands of UNIX such as echo, cat, grep, cd, ls,
rm, and open. These can be called like regular functions in josh.

Example:
```
string ret = cat("out.txt")
```

## List and string operations
There are standard library functions for common operations like comparison,
concatenation, mapping, searching, and more in lists and strings. Some examples
can be found below.

### Comparison
Example:
```
[int] myList = [1, 2, 3];
[float] myList2 = [1, 2, 3, 4];
if (equals(myList, myList2)) (echo(“They are equal!”));
```

### Concatenation
Example:
```
concat([1, 2, 3], [4, 5, 6]);    {| result: [1, 2, 3, 4, 5, 6] |}
strcat("abc", "123");            {| result: "abc123" |}
```

## Conversions
The standard library implements functions for converting between major data
types where it makes sense.

Example:
```
int myInt = 5;
string myGoodString = "3.14159";
string myBadString = "hello";

int_to_string(5)              {| "5"     |}
string_to_int(myGoodString)   {| 3.14159 |}
string_to_int(myBadString)    {| ERROR   |}
```

## I/O
See section on Bash. All input and output is done through standard library
functions which either mimic GNU commands or directly interface with them.

Example:
```
string text_of_file = cat("myfile.txt");
string all_lowercase = toLower(text_of_file);
bash("echo ", all_lowercase, " > myfile.txt");
```

There are many ways to achieve the same result.

# Appendix
## Scanner
```
{ open Parser
  open Scanf }

let letter = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = (letter | '_') (letter | digit | '_')* (* keywords??? *)
let string = '"' ( (ascii | escape)* as lem ) '"'
let char = ''' ( ascii ) ''' (* digit? *)
let float = ('-')? ((digit+) ['.'] digit*) | ((digit*) ['.'] digit+)
let int = ('-')? digit+
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
  | whitespace { token lexbuf }
  | "{|"     { comment lexbuf }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | '.'      { DOT }
  | ';'      { SEMI }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { MULT }
  | '/'      { DIV }
  | '%'      { MOD }
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
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "record" { RECORD }
  | int as lem { INTLIT(int_of_string lem) }
  | float as lem { FLOATLIT(float_of_string lem)}
  | char as lem { CHARLIT(String.get lem 1)}
  | string { STRLIT(Scanf.unescaped lem)}
  | escape_char as lem { CHARLIT(String.get (Scanf.unescaped lem) 1) }
  | id as lem { ID(lem) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    | "|}"  { token lexbuf }
    | _     { comment lexbuf }
```

## Parser
```
/* Ocamlyacc parser for Josh */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA DOT
%token PLUS MINUS ASSIGN MULT DIV MOD
%token EQ NEQ LT GT LEQ GEQ AND OR NOT
%token IF ELSE FOR IN WHILE BREAK CONTINUE RETURN
%token INT BOOL FLOAT STRING VOID CHAR
%token RECORD
%token <int> INTLIT
%token <char> CHARLIT
%token <string> STRLIT
%token <bool> BOOLLIT
%token TRUE FALSE
%token <float> FLOATLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left NOT
%left OR
%left AND
%left EQ NEQ
%left LT LEQ
%left GT GEQ
%left MOD
%left MULT DIV
%left PLUS MINUS

%%

program:
  top_level_list EOF { $1 }

top_level_list:
  { [] }
  | top_level top_level_list { $1::$2 }

top_level:
    stmt { Stmt $1 }
  | fdecl { Fdecl $1 }

typ:
  INT       { Int  }
  | BOOL    { Bool }
  | FLOAT   { Float }
  | CHAR    { Char }
  | STRING  { String }
  | LBRACK typ RBRACK { ListT($2) }
  | VOID    { Void }
  | ID { RecordType($1) }
  | typ ID LPAREN opts_list RPAREN { FunkType($2, $4, $1) }
      /*
      record Thing {
          string name;
          int fptr(char c, float d);
      }
      record Thing myThing = { "josh", func };
      myThing.func(c, d);

      */

stmt_list:
    /* nothing */               { [] }
    | stmt stmt_list  { $1::$2 }

fdecl:
    typ ID LPAREN opts_list RPAREN LBRACE stmt_list RBRACE
    { { id=$2; params=$4; body=$7; return_type=$1 } }

stmt:
  expr SEMI                                          { Expr $1         }
  | vdecl SEMI                                       { Vdecl $1 }
  | LBRACE stmt_list RBRACE                          { Block $2        }
  | IF LPAREN expr RPAREN stmt ELSE stmt   { If ($3, $5, $7) }
  | IF LPAREN expr RPAREN stmt             { If ($3, $5, Expr(Noexpr)) }
  | FOR LPAREN ID IN expr RPAREN stmt { For ($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt               { While ($3,$5)   }
  | RECORD ID LBRACE opts_list RBRACE SEMI { RecordDef($2, $4) }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | CONTINUE SEMI { Continue }
  | BREAK SEMI    { Break }

expr_list:
    /* nothing */   { [] }
    | expr_list COMMA expr { $3::$1 }

expr:
  /* literals */
   BOOLLIT                      { BoolLit $1  }
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
  | expr DIV expr       { Binop ($1, Div, $3)   }
  | expr MULT expr      { Binop ($1, Mul, $3)   }
  | expr MOD expr        { Binop ($1, Mod, $3)  }
  /* equality */
  | expr EQ expr        { Binop ($1, Equal, $3) }
  | expr NEQ expr       { Binop ($1, Neq, $3)   }
  | expr LT expr        { Binop ($1, Less, $3)  }
  | expr LEQ expr       { Binop ($1, Leq, $3)  }
  | expr GT expr        { Binop ($1, Greater, $3)  }
  | expr GEQ expr       { Binop ($1, Geq, $3)  }
  /* logical */
  | expr AND expr       { Binop ($1, And, $3)   }
  | expr OR expr        { Binop ($1, Or, $3)    }
  | NOT expr        { Unop (Not, $2)    }
  | ID ASSIGN expr           { Assign ($1, $3)       }
  | LPAREN expr RPAREN       { $2                    }
  /* list */
  | LBRACK expr_list RBRACK { ListLit(List.rev $2)  }
  | expr LBRACK expr RBRACK { ListAccess($1, $3) }
  | expr DOT ID { RecordAccess($1, $3) }
  /* record instantiation */
  | ID LBRACE actuals_list RBRACE { RecordCreate($1, $3) }
  /* mutation */
  | expr DOT ID ASSIGN expr { MutateRecord(($1,$3), $5) }
  | expr LBRACK expr RBRACK ASSIGN expr { MutateList(($1,$3), $6) }
  /* function call */
  | ID LPAREN actuals_list RPAREN { Call($1, $3) }
  | expr DOT ID LPAREN actuals_list RPAREN { CallRecord(($1,$3), $5) }
  | expr LBRACK expr RBRACK LPAREN actuals_list RPAREN { CallList(($1,$3), $6) }

vdecl:
    typ ID { Declare($1, $2) }
  | typ ID ASSIGN expr { Initialize($1, $2, $4)}

/* for record field and function argument lists */
opts:
    typ ID  { [Opt($1,$2)] }
    | opts COMMA typ ID { Opt($3,$4) :: $1 }

opts_list:
    /* nothing */ { [] }
  | opts { List.rev $1 }

/* for instantiating records and calling functions */
actuals_list:
    /* nothing */ { [] }
    | actuals_list COMMA actual { $3::$1 }

actual:
    expr { Actual $1 }
```

# References
C Reference Manual: <https://www.bell-labs.com/usr/dmr/www/cman.pdf>

Rusty Language Reference Manual:
<http://www.cs.columbia.edu/~sedwards/classes/2016/4115-fall/lrms/rusty.pdf>
