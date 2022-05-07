open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful, throws an
 * exception if something is wrong. *)

(* Check each top-level declaration. *)
let check decls =

    (* Need to check:
        * Variables are in scope.
        * Function return type matches type of return expression.
        * Variable assignment.
        * Binary operations.
        * Parameter types match formal types.
        * If conditions are Boolean.
        * List access expressions are int types accessing list types.
        * Record access fields are in formals list of record definition.
        * Record access is accessing a record type.
        * Scoping of record declaration.
    *)

    (* Check for duplicates. *)
    let check_binds (formals: opt list) =
        let rec dups = function
            [] -> ()
          | (Opt(_,n1) :: Opt(_,n2) :: _) when n1 = n2 ->
                  raise (Failure ("duplicate parameter " ^ n1))
          | _ :: t -> dups t
        in dups (List.sort (fun (Opt(_,a)) (Opt(_,b)) -> compare a b) formals)
    in


    let built_in_decls =
      let add_decl map (fname, formals, rtyp) = 
        StringMap.add fname {
          rtyp = rtyp;
          fname = fname;
          formals = formals;
          body = []
        } map
      in
      List.fold_left add_decl StringMap.empty [
        ("echoi", [Opt(Int, "i")], Void);
        ("echo", [Opt(String, "s")], Void)
      ]
    in

    let built_in_recs = StringMap.empty
    in

    let add_func map fd =
      let built_in_err = "function " ^ fd.fname ^ " may not be defined"
      and dup_err = "duplicate function " ^ fd.fname
      and make_err er = raise (Failure er)
      and n = fd.fname (* Name of the function *)
      in match fd with (* No duplicate functions or redefinitions of built-ins *)
        _ when StringMap.mem n built_in_decls -> make_err built_in_err
      | _ when StringMap.mem n map -> make_err dup_err
      | _ ->  StringMap.add n fd map
    in

    let add_rec map rd =
      let n = fst rd in
      let built_in_err = "record " ^ n ^ " may not be defined"
      and dup_err = "duplicate record " ^ n
      and make_err er = raise (Failure er)
      in match rd with
        _ when StringMap.mem n built_in_recs -> make_err built_in_err
      | _ when StringMap.mem n map -> make_err dup_err
      | _ ->  StringMap.add n rd map
    in

    let globals = StringMap.empty
    in

    (* Find all the function definitions in the program. *)
    let functions = List.filter_map
                    (function Fdecl f -> Some f | _ -> None)
                    decls
    in

    (* Find all top-level record definitions in the program. *)
    let recs = List.filter_map
               (function Stmt (RecordDef(r, f)) -> Some (r, f) | _ -> None)
               decls
    in

    (* Collect all function names into one symbol table *)
    let function_decls = List.fold_left add_func built_in_decls functions
    in

    (* Collect all record names into one symbol table *)
    let rec_defs = List.fold_left add_rec built_in_recs recs
    in

    (* Return a function from our symbol table *)
    let find_func s =
      try StringMap.find s function_decls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in

    (* let _ = find_func "main" in (* Ensure "main" is defined *) *)

    let find_rec s =
      try StringMap.find s rec_defs
      with Not_found -> raise (Failure ("unrecognized record " ^ s))
    in

    (* Return a variable type from a local symbol table *)
    let type_of_identifier env s =
      try StringMap.find s env
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if (string_of_typ lvaluet = string_of_typ rvaluet) || (rvaluet = EmptyList) 
	then lvaluet else raise (Failure err)
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr env = function
        Noexpr -> (Void, SNoexpr)
      | IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | CharLit l -> (Char, SCharLit l)
      | StrLit l -> (String, SStrLit l)
      | Id var -> (type_of_identifier env var, SId var)
      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr env e1
        and (t2, e2') = check_expr env e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_bop op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
          (* can we add chars, strings, lists, etc? *)
              Add | Sub | Mul | Div | Mod when t1 = Int -> Int
            | Add | Sub | Mul | Div when t1 = Float -> Float
            | Equal | Neq -> Bool
            | Less | Leq | Greater | Geq when t1 = Int -> Bool
            | Less | Leq | Greater | Geq when t1 = Float -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Unop(op, e) as ex ->
        let (t, e') = check_expr env e in
        let err = "illegal unary operator " ^ string_of_uop op ^
                  string_of_typ t ^ " in " ^ string_of_expr ex
        in
        let t' = match op with
            Not when t = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t', SUnop(op, (t, e')))
      | Assign(var, e) as ex ->
        let lt = type_of_identifier env var
        and (rt, e') = check_expr env e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))
      | RecordCreate(rt, actuals) as ex ->
        let (_, formals) = find_rec rt in
        let formals_length = List.length formals in
        if List.length actuals != formals_length then
            raise (Failure ("expecting " ^ string_of_int formals_length ^
                            "fields in " ^ string_of_expr ex))
        else
        let fts = List.map (fun (Opt(t,_)) -> t) formals in
        let sactuals = List.map (check_expr env) actuals in
        if List.map fst sactuals <> fts then
            raise (Failure ("field types do not match in " ^ string_of_expr ex))
            (* TODO: better error message for above *)
        else (RecordType(rt), SRecordCreate(rt, sactuals))
      | RecordAccess(e, field) as ex ->
        let (t, e') = check_expr env e in
        (match t with
            RecordType(rt) ->
              let (_, formals) = find_rec rt in
              let Opt(ft,_) = try List.find (fun (Opt(_,i)) -> i = field) formals
                  with Not_found -> raise (Failure ("field " ^ field ^
                                           " not a member of record " ^ rt ^
                                           " in " ^ string_of_expr ex))
              in (ft, SRecordAccess((t, e'), field))
          | _ -> raise (Failure ("expected RecordType in " ^ string_of_expr ex)))
        (* TODO: better error message for above *)
      | MutateRecord((e1, field), e2) as ex ->
        let (t1, e1') = check_expr env e1 in
        (match t1 with
            RecordType(rt) ->
              let (_, formals) = find_rec rt in
              let Opt(ft,_) = try List.find (fun (Opt(_,i)) -> i = field) formals
                  with Not_found -> raise (Failure ("field " ^ field ^
                                           " not a member of record " ^ rt ^
                                           " in " ^ string_of_expr ex))
              in
              let (t2, e2') = check_expr env e2 in
              if ft != t2 then
                  raise (Failure ("expected type " ^ string_of_typ ft ^
                                  " but got type " ^ string_of_typ t2 ^
                                  " in " ^ string_of_expr ex))
              else (ft, SMutateRecord(((t1, e1'), field), (t2, e2')))
          | _ -> raise (Failure ("expected RecordType in " ^ string_of_expr ex)))
        (* TODO: better error message for above *)
      | ListLit(actuals) as ex ->
        let sactuals =
            List.fold_right
            (fun e acc ->
                let (t, e') = check_expr env e
                in match acc with
                    [] -> [(t, e')]
                  | (t',_)::_ when t = t' -> (t,e')::acc
                  | (t',_)::_ ->
                    raise (Failure ("non-matching list element types " ^
                                    string_of_typ t ^ ", " ^ string_of_typ t'
                                    ^ " in " ^ string_of_expr ex)))
            actuals []
        in if List.length sactuals > 0 then (ListT(fst (List.hd sactuals)), SListLit(sactuals))
		else (EmptyList, SNoexpr)
      | ListAccess(e1, e2) as ex ->
        let (t2, e2') = check_expr env e2 in
        let (t1, e1') = check_expr env e1 in
        (match t1 with
            ListT(t) when t2 = Int -> (t, SListAccess((t1, e1'), (t2, e2')))
          | ListT(t) ->
                  raise (Failure ("list index found " ^
                                  string_of_typ t ^ ", expected Int in "
                                  ^ string_of_expr ex))
          | t ->
                  raise (Failure ("list expression found " ^
                                  string_of_typ t ^ ", expected ListT in "
                                  ^ string_of_expr ex)))
      | MutateList((e1, i), e2) as ex ->
        let (it, i') = check_expr env i in
        let (t1, e1') = check_expr env e1 in
        (match t1 with
            ListT(t) when it = Int ->
                let (t2, e2') = check_expr env e2 in
                if t != t2 then
                  raise (Failure ("list of type " ^ string_of_typ t ^
                                  " cannot be assigned to type " ^
                                  string_of_typ t2 ^ " in " ^ string_of_expr ex))
                else (t2, SMutateList(((t1, e1'), (it, i')), (t2, e2')))
          | ListT(t) ->
                  raise (Failure ("list index found " ^
                                  string_of_typ t ^ ", expected Int in "
                                  ^ string_of_expr ex))
          | t ->
                  raise (Failure ("list expression found " ^
                                  string_of_typ t ^ ", expected ListT in "
                                  ^ string_of_expr ex)))
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr env e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call (List.map (fun (Opt(a,b)) -> (a,b)) fd.formals) args
          in (fd.rtyp, SCall(fname, args'))
          (* TODO *)
      | CallRecord((e, field), args) -> raise (Failure "not implemented")
      | CallList((e1, e2), args) -> raise (Failure "not implemented")
    in

    let check_bool_expr env e =
      let (t, e') = check_expr env e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let update_env env = function
        Vdecl(Declare(t, v)) -> StringMap.add v t env
      | Vdecl(Initialize(t, v, _)) -> StringMap.add v t env
      | _ -> env
    in

    let check_vdecl env = function
        Declare(t, v) -> SDeclare(t, v)
      | Initialize(lt, v, e) as ex ->
          let (rt, e') = check_expr env e
          in
          let err = "illegal initialization " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_vdecl ex
          in
          ignore (check_assign lt rt err);
          SInitialize(lt, v, (rt, e'))
    in

    let rec check_stmt_list env = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list env (sl @ sl') (* Flatten blocks *)
      | s :: sl ->
              let env' = update_env env s
              in
              check_stmt env s :: check_stmt_list env' sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
      and check_stmt env = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list env sl)
      | Expr e -> SExpr (check_expr env e)
      | Vdecl v -> SVdecl (check_vdecl env v)
      | If(e, st1, st2) ->
        SIf(check_bool_expr env e, check_stmt env st1, check_stmt env st2)
      | For(i, e, st) ->
        SFor(i, check_expr env e, check_stmt env st)
      | While(e, st) ->
        SWhile(check_bool_expr env e, check_stmt env st)
      | RecordDef(i, formals) -> SRecordDef(i, formals)
      | Return e ->
        (* TODO: throw error if you try to return while not inside a function *)
        let rtyp = try StringMap.find "__rtyp__" env
           with Not_found -> raise (Failure ("return outside function"))
        in
        let (t, e') = check_expr env e in
        if t = rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ rtyp ^ " in " ^ string_of_expr e))
      | Continue -> SContinue
      | Break -> SBreak
    in

    let check_func env func =
      (* Make sure no formals are duplicates *)
      check_binds func.formals;

      (* Build local symbol table of variables for this function *)
      let env' = List.fold_left (fun m (Opt(ty, name)) -> StringMap.add name ty m)
          env ((Opt(func.rtyp, "__rtyp__"))::func.formals)
      in

      SFdecl ({
        srtyp = func.rtyp;
        sfname = func.fname;
        sformals = func.formals;
        sbody = check_stmt_list env' func.body
      })
    (* End of check_func *)
    in

    let stmts =
        List.filter_map
        (function Stmt s -> Some s | _ -> None)
         decls
    in

    let env = globals
    in
    (List.map (check_func env) functions)
    @
    (List.map (fun s -> SStmt(s)) (check_stmt_list env stmts))
