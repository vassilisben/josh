open Ast

type sexpr = typ * sx
and sx =
  | SNoexpr
  | SIntLit of int
  | SBoolLit of bool
  | SFloatLit of float
  | SCharLit of char
  | SStrLit of string
  | SId of string    (* figure this out *)
  | SBinop of sexpr * bop * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SRecordCreate of id * sexpr list
  | SRecordAccess of sexpr * id
  | SMutateRecord of (sexpr * id) * sexpr
  | SListLit of sexpr list
  | SListAccess of sexpr * sexpr
  | SMutateList of (sexpr * sexpr) * sexpr
  | SCall of id * sexpr list
  | SCallRecord of (sexpr * id) * sexpr list
  | SCallList of (sexpr * sexpr) * sexpr list

type svdecl =
  | SDeclare of typ * id
  | SInitialize of typ * id * sexpr

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SVdecl of svdecl
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SRecordDef of id * opt list
  | SReturn of sexpr
  | SContinue
  | SBreak

type sfdecl = {
  srtyp: typ;
  sfname: id;
  sformals: opt list;
  sbody: sstmt list;
}

type stop_level =
  | SStmt of sstmt
  | SFdecl of sfdecl

type sprogram = stop_level list

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ " " ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SNoexpr -> ""
  | SFloatLit(l) -> string_of_float l
  | SCharLit(l) -> "'" ^ Char.escaped l ^ "'"
  | SStrLit(l) -> "\"" ^ String.escaped l ^ "\""
  | SRecordCreate(rec_name, actuals) ->
    rec_name ^ " { " ^ String.concat ", " (List.map string_of_sexpr actuals) ^ " }"
  | SRecordAccess(r, field) -> string_of_sexpr r ^ "." ^ field
  | SMutateRecord((r, field), e) -> string_of_sexpr r ^ "." ^ field ^ " = " ^ string_of_sexpr e
  | SListLit(l) -> "[" ^ String.concat ", " (List.map string_of_sexpr l) ^ "]"
  | SListAccess(lst, ind) -> string_of_sexpr lst ^ "[" ^ string_of_sexpr ind ^ "]"
  | SMutateList((lst, ind), e) -> string_of_sexpr lst ^ "[" ^ string_of_sexpr ind ^ "] = " ^ string_of_sexpr e
  | SCall(f, actuals) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr actuals) ^ ")"
  | SCallRecord((r, field), actuals) ->
    string_of_sexpr r ^ "." ^ field ^
    "(" ^ String.concat ", " (List.map string_of_sexpr actuals) ^ ")"
  | SCallList((lst, ind), actuals) ->
    string_of_sexpr lst ^ "[" ^ string_of_sexpr ind ^ "]" ^
    "(" ^ String.concat ", " (List.map string_of_sexpr actuals) ^ ")"
  ) ^ ")"

let string_of_svdecl = function
  | SDeclare(t, id) -> string_of_typ t ^ " " ^ id ^ ";"
  | SInitialize(t, id, sexpr) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr sexpr ^ ";"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";";
  | SVdecl(decl) -> string_of_svdecl decl
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, stmt) -> "for (" ^ string_of_sexpr e1 ^ "; " ^ string_of_sexpr e2 ^ "; " ^ string_of_sexpr e3 ^ ") " ^ string_of_sstmt stmt
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SRecordDef(rec_name, formals) ->
    "record " ^ rec_name ^ " {\n  " ^ String.concat ",\n  " (List.map string_of_opt formals) ^ "\n};"
  | SReturn(e) -> "return " ^ string_of_sexpr e ^ ";"
  | SContinue -> "continue;"
  | SBreak -> "break;"

let string_of_sfdecl f =
    string_of_typ f.srtyp ^ " " ^
    f.sfname ^ "(" ^
    string_of_opt_list f.sformals ^ ") {\n  " ^
    String.concat "\n  " (List.map string_of_sstmt f.sbody)
    ^ "\n}"

let string_of_sprogram sdecls =
    let stringify = function
      | SStmt(t) -> string_of_sstmt t
      | SFdecl(t) -> string_of_sfdecl t
    in
  "\n\nParsed program: \n\n" ^
  String.concat "\n\n" (List.map stringify sdecls) ^
  "\n"
