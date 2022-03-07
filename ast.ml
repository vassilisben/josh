
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
  | RecordType of id
  | FunkType of id * opt list * typ
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
  | RecordCreate of actual list
  | RecordAccess of expr * id
  | MutateRecord of (expr * id) * expr
  | ListLit of expr list
  | ListAccess of expr * expr
  | MutateList of (expr * expr) * expr
  | Call of id * actual list
  | CallRecord of (expr * id) * actual list
  | CallList of (expr * expr) * actual list
and actual = Actual of expr

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

type fdecl = {
  id: id;
  params: opt list;
  body: stmt list;
  return_type: typ;
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
  | RecordType(t) -> "record " ^ t
  | ListT(t) -> "list " ^ string_of_typ t
  | _ -> "add more stuff"

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
  | _ -> "fuck u"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | _ -> "add more stuff"

let string_of_vdecl = function
  | Declare(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | Initialize(t, id, expr) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr expr ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Vdecl(decl) -> string_of_vdecl decl
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | _ -> "add more stuff"

let string_of_opt = function
    | Opt(t, id) -> string_of_typ t ^ " " ^ id
let string_of_opt_list l = String.concat ", " (List.map string_of_opt l)

let string_of_fdecl f =
    string_of_typ f.return_type ^ " " ^
    f.id ^ "(" ^
    string_of_opt_list f.params ^ ")" ^
    String.concat "\n" (List.map string_of_stmt f.body)

let string_of_program decls =
    let stringify = function
      | Stmt(t) -> string_of_stmt t
      | Fdecl(t) -> string_of_fdecl t
    in
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map stringify decls) ^
  "\n"
