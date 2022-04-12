
type bop = Add | Sub | Mul | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Not

type id = string

type typ =
  | Int
  | Bool
  | Float
  | Char
  | String
  | ListT of typ
  | Void
  | EmptyList
  | RecordType of id
  | FunkType of typ list * typ

and opt = Opt of typ * id

type expr =
  | Noexpr
  | IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | CharLit of char
  | StrLit of string
  | Id of string    (* figure this out *)
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | Assign of string * expr
  | RecordCreate of id * expr list
  | RecordAccess of expr * id
  | MutateRecord of (expr * id) * expr
  | ListLit of expr list
  | ListAccess of expr * expr
  | MutateList of (expr * expr) * expr
  | Call of id * expr list
  | CallRecord of (expr * id) * expr list
  | CallList of (expr * expr) * expr list

type vdecl =
  | Declare of typ * id
  | Initialize of typ * id * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | Vdecl of vdecl
  | If of expr * stmt * stmt
  | For of id * expr * stmt
  | While of expr * stmt
  | RecordDef of id * opt list
  | Return of expr
  | Continue
  | Break

type fdecl = {
  rtyp: typ;
  fname: id;
  formals: opt list;
  body: stmt list;
}

type top_level =
  | Stmt of stmt
  | Fdecl of fdecl

type program = top_level list

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Char -> "char"
  | Void -> "void"
  | EmptyList -> "EmptyList"
  | RecordType(t) -> "record " ^ t
  | ListT(t) -> "[" ^ (string_of_typ t) ^ "]"
  | FunkType(types, return_typ) ->
    string_of_typ return_typ ^ "(" ^ String.concat ", " (List.map string_of_typ types) ^ ")"
and string_of_opt = function
  | Opt(t, id) -> string_of_typ t ^ " " ^ id
and string_of_opt_list l = String.concat ", " (List.map string_of_opt l)

let string_of_bop = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

let string_of_uop = function
    Not -> "not"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ " " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> "'" ^ Char.escaped l ^ "'"
  | StrLit(l) -> "\"" ^ String.escaped l ^ "\""
  | RecordCreate(rec_name, actuals) ->
    rec_name ^ " { " ^ String.concat ", " (List.map string_of_expr actuals) ^ " }"
  | RecordAccess(r, field) -> string_of_expr r ^ "." ^ field
  | MutateRecord((r, field), e) -> string_of_expr r ^ "." ^ field ^ " = " ^ string_of_expr e
  | ListLit(l) -> "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | ListAccess(lst, ind) -> string_of_expr lst ^ "[" ^ string_of_expr ind ^ "]"
  | MutateList((lst, ind), e) -> string_of_expr lst ^ "[" ^ string_of_expr ind ^ "] = " ^ string_of_expr e
  | Call(f, actuals) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr actuals) ^ ")"
  | CallRecord((r, field), actuals) ->
    string_of_expr r ^ "." ^ field ^
    "(" ^ String.concat ", " (List.map string_of_expr actuals) ^ ")"
  | CallList((lst, ind), actuals) ->
    string_of_expr lst ^ "[" ^ string_of_expr ind ^ "]" ^
    "(" ^ String.concat ", " (List.map string_of_expr actuals) ^ ")"

let string_of_vdecl = function
  | Declare(t, id) -> string_of_typ t ^ " " ^ id ^ ";"
  | Initialize(t, id, expr) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr expr ^ ";"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";";
  | Vdecl(decl) -> string_of_vdecl decl
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(i, iter, stmt) -> "for (" ^ i ^ " in " ^ string_of_expr iter ^ ") " ^ string_of_stmt stmt
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | RecordDef(rec_name, formals) ->
    "record " ^ rec_name ^ " {\n  " ^ String.concat ",\n  " (List.map string_of_opt formals) ^ "\n};"
  | Return(e) -> "return " ^ string_of_expr e ^ ";"
  | Continue -> "continue;"
  | Break -> "break;"

let string_of_fdecl f =
    string_of_typ f.rtyp ^ " " ^
    f.fname ^ "(" ^
    string_of_opt_list f.formals ^ ") {\n  " ^
    String.concat "\n  " (List.map string_of_stmt f.body)
    ^ "\n}"

let string_of_program decls =
    let stringify = function
      | Stmt(t) -> string_of_stmt t
      | Fdecl(t) -> string_of_fdecl t
    in
  "\n\nParsed program: \n\n" ^
  String.concat "\n\n" (List.map stringify decls) ^
  "\n"
