module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* TODO
 * records
 *   pass record inside function (fix formals)
 * lists...
 * record access
 * standard library things?
 * write the report
 * make the video
 *)

let translate top_level =
  (* boilerplate *)
  let context = L.global_context() in
  let josh_module = L.create_module context "Josh" in

  (* types *)
  let i32_t       = L.i32_type    context
  and i64_t       = L.i64_type    context
  and i8_t        = L.i8_type     context
  and i1_t        = L.i1_type     context
  and float_type  = L.float_type  context
  and void_type   = L.void_type context
  and array_type  = (L.array_type)
  and record_type = (L.struct_type context) in

  let rec ltype_of_typ typ arr = match typ with
      A.Int           -> i32_t
    | A.Bool          -> i1_t
    | A.Char          -> i8_t
    | A.Float         -> float_type
    | A.Void          -> void_type
    | A.String        -> L.pointer_type (L.i8_type context)
    | A.RecordType id -> record_type arr
    | A.ListT (ty,_)  -> array_type (ltype_of_typ ty [||]) 0 (* we will malloc later *)
    | _ -> i32_t
  in

  (* Get all functions declarations in source *)
  let functions = List.filter_map
                  (function SFdecl f -> Some f | _ -> None)
                  top_level
  in
  (* TODO: get top level vdecls *)
  let record_defs = List.filter_map (function
                                      | SStmt(sstmt) -> (match sstmt with
                                        | SRecordDef(id, opts_list) -> Some (SRecordDef(id, opts_list))
                                        | _ -> None)
                                      | _ -> None) top_level
  in

  let rec build_global_expr env ((_, e) : sexpr) = match e with
      SIntLit i                      -> L.const_int i32_t i
    | SBoolLit b                     -> L.const_int i1_t (if b then 1 else 0)
    | SStrLit s                      -> L.const_string context s
    (*| SId id                         -> build_global_expr env (StringMap.find id env)*)
    | SBinop (e1, op, e2) ->
        let e1' = build_global_expr env e1
        and e2' = build_global_expr env e2 in
        (match op with
            A.Add     -> L.const_add
          | A.Sub     -> L.const_sub
          | A.Mul     -> L.const_mul
          | A.Div     -> L.const_sdiv
          | A.Mod     -> L.const_srem
          | A.And     -> L.const_and
          | A.Or      -> L.const_or
          | A.Equal   -> L.const_icmp L.Icmp.Eq
          | A.Neq     -> L.const_icmp L.Icmp.Ne
          | A.Less    -> L.const_icmp L.Icmp.Slt
          | A.Leq     -> L.const_icmp L.Icmp.Sle
          | A.Greater -> L.const_icmp L.Icmp.Sgt
          | A.Geq     -> L.const_icmp L.Icmp.Sge
        ) e1' e2'
    | SListLit(sactuals) ->
            let t = ltype_of_typ (fst (List.hd sactuals)) [||] in
            let values = Array.of_list (List.map (build_global_expr env) sactuals) in
            L.const_array t values
    | _ -> raise (Failure ("Constant cannot have that type"))
  in

  let globals = List.filter_map (function
                                      | SStmt(sstmt) -> (match sstmt with
                                        | SVdecl s -> Some s
                                        | _ -> None)
                                      | _ -> None) top_level
  in

  let global_vars : L.llvalue StringMap.t =
    let global_var (m,env) = function
        | SDeclare (t, id) ->
          let init = (match t with
              A.Int -> L.const_int (ltype_of_typ t [||]) 0
            | A.Bool -> L.const_int (ltype_of_typ t [||]) 0
            | A.Char -> L.const_int (ltype_of_typ t [||]) 0
            | A.Float -> L.const_float (ltype_of_typ t [||]) 0.0
            | A.String -> L.const_string context "")
          in (StringMap.add id (L.define_global id init josh_module) m,
              env)
        | SInitialize (t, id, e) ->
          let init = build_global_expr env e
          in (StringMap.add id (L.define_global id init josh_module) m,
              StringMap.add id init env)
          in
    fst (List.fold_left global_var (StringMap.empty,StringMap.empty) globals) in
	(****** STANDARD LIBRARY CALLS *******)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t josh_module in

  let bash_t : L.lltype =
    L.function_type i32_t [| L.pointer_type i8_t |] in
  let bash_func : L.llvalue =
    L.declare_function "fork_exec" bash_t josh_module in

  let subset_t : L.lltype =
    L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t; i32_t; i32_t |] in
  let subset_func : L.llvalue =
    L.declare_function "josh_subset" subset_t josh_module in

  let strcmp_t : L.lltype =
    L.function_type i32_t [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let strcmp_func : L.llvalue =
    L.declare_function "josh_strcmp" strcmp_t josh_module in

  let concat_t : L.lltype =
    L.function_type (L.pointer_type i8_t) [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let concat_func : L.llvalue =
    L.declare_function "josh_concat" concat_t josh_module in

	let sqrt_t : L.lltype =
		L.function_type float_type [| i32_t |] in
	let sqrt_func : L.llvalue =
		L.declare_function "josh_sqrt" sqrt_t josh_module in

	let fsqrt_t : L.lltype =
		L.function_type float_type [| float_type |] in
	let fsqrt_func : L.llvalue =
		L.declare_function "josh_sqrt2" fsqrt_t josh_module in

	let pow_t : L.lltype =
		L.function_type i32_t [| i32_t; i32_t |] in
	let pow_func : L.llvalue =
		L.declare_function "josh_pow" pow_t josh_module in

	let fpow_t : L.lltype =
		L.function_type float_type [| float_type; float_type |] in
	let fpow_func : L.llvalue =
		L.declare_function "josh_pow2" fpow_t josh_module in

	let itf_t : L.lltype =
		L.function_type float_type [| i32_t |] in
	let itf_func : L.llvalue =
		L.declare_function "int_to_float" itf_t josh_module in

	let fti_t : L.lltype =
		L.function_type i32_t [| float_type |] in
	let fti_func : L.llvalue =
		L.declare_function "float_to_int" fti_t josh_module in

		(* END OF STANDARD LIBRARY *)

  (* create llvm function declarations using the function declarations from source *)
  let function_decls : (L.llvalue * sfdecl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t [||]) (List.map (fun (A.Opt(t,id)) -> (t,id)) fdecl.sformals))
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp [||]) formal_types in
      StringMap.add name (L.define_function name ftype josh_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* For each llvm function declaration, create the corresponding llvm 3-addr code function body *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
		and flt_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    let rec lookup n = function
        [] -> raise (Failure ("Not found: " ^ n))
      | (frame::frames) ->
        try StringMap.find n frame
        with Not_found -> lookup n frames
    in

    (* create space for each function paramter *)
    let add_formal m (t, n) p =
      L.set_value_name n p;

      (* TODO: expand to support records/lists *)
      let local = match t with
        | (A.RecordType id) as r ->
            let global_type = (L.type_by_name josh_module "Person") in (match global_type with
                Some t -> L.build_alloca (ltype_of_typ r (L.struct_element_types t)) n builder
              | None -> raise(Failure("Unrecognized record type")))
        | _ -> L.build_alloca (ltype_of_typ t [||]) n builder
      in
      ignore (L.build_store p local builder);
      StringMap.add n local m
    in

    let formals = List.fold_left2 add_formal StringMap.empty (List.map (fun (A.Opt(t, id)) -> (t,id)) fdecl.sformals)
        (Array.to_list (L.params the_function)) in

    let add_local m (t, n) =
      (* create space for each local var declaration *)
        let local_var = match t with
        | A.RecordType id ->
            let global_type = (L.type_by_name josh_module id) in (match global_type with
              Some t -> L.build_alloca t n builder
              | None -> raise(Failure("Unrecognized record type")))
        | _ -> L.build_alloca (ltype_of_typ t [||]) n builder
        in StringMap.add n local_var m
      in

    let build_record_ltypes_array sexpr_list =
      let rec build_record_ptr_helper lltype_list sexpr_list = match sexpr_list with
      | [] -> lltype_list
      | hd::tl ->
          let ((typ,_) : sexpr) = hd in (build_record_ptr_helper ((ltype_of_typ typ [||])::lltype_list) tl) (* no support for nested records yet *)
      in

      let record_types_list = (build_record_ptr_helper [] sexpr_list) in
      let find_ith_type i = (List.nth record_types_list i) in

      Array.init (List.length record_types_list) find_ith_type
    in

    let get_member_index record_name member =
      let rec aux record_defs_left record_name member = match record_defs_left with
      | [] -> raise(Failure("Couldn't find record type"))
      | hd::tl -> let (SRecordDef(rec_name, formals)) = hd in match ("record " ^ rec_name) with
        | record_name -> let rec formals_aux formals_left index = match formals_left with
          | [] -> raise(Failure("Couldn't find member"))
          | hd::tl -> let A.Opt(_,id) = hd in match id with
            | member -> index
            | _ -> formals_aux tl (index+1)
          in formals_aux formals 0
        | _ -> (aux tl record_name member)
      in
      aux record_defs record_name member
    in

    let rec build_expr builder ((frame::frames) as env) ((_, e) : sexpr) = match e with
        SIntLit i                         -> L.const_int i32_t i
      | SRecordCreate (id, sexpr_list)    -> let global_type = (L.type_by_name josh_module id) in (match global_type with
                                               Some t -> (L.build_alloca t id builder) (*L.build_alloca (record_type (build_record_ltypes_array sexpr_list)) id builder*)
                                               | None -> raise(Failure("Unrecognized record type")))
      | SMutateRecord(((typ, SId(id)), member), new_value) -> let record_ptr = (lookup id env) in
                                                              let member_ptr = L.build_struct_gep record_ptr (get_member_index typ member) member builder in
                                                              L.build_store (build_expr builder env new_value) member_ptr builder
      | SRecordAccess((typ, SId(id)), member) -> let record_ptr = (lookup id env) in
                                                let member_ptr = L.build_struct_gep record_ptr (get_member_index typ member) member builder in
                                                L.build_load member_ptr member builder

      | SBoolLit b                        -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s                         -> L.build_global_stringptr s "tmp" builder
      | SId id                            -> L.build_load (lookup id env) id builder
      | SAssign (s, e)                    -> let e' = build_expr builder env e in
        ignore(L.build_store e' (lookup s env) builder); e'
      | SBinop (e1, op, e2) ->
          let e1' = build_expr builder env e1
          and e2' = build_expr builder env e2 in
          (match op with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mul     -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.Mod     -> L.build_srem
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder
      | SListLit(exprs) ->
            let t = ltype_of_typ (fst (List.hd exprs)) [||] in
            let n_bytes = L.const_mul (L.const_int i32_t (List.length exprs))
                                      (L.size_of (ltype_of_typ (fst (List.hd exprs)) [||]))
            in
            let arr = L.build_array_malloc t n_bytes "tmp" builder in
            fst (List.fold_left
              (fun (agg,idx) e ->
                  (*(L.build_insertvalue agg
                    (build_expr builder env e) idx "tmp" builder,*)
                  (let e' = build_expr builder env e in
                  let p = L.build_gep agg [|L.const_int i32_t idx; L.const_int i32_t 0|] "tmp" builder in
                  L.build_store p e' builder,
                  idx+1)) (arr, 0) exprs)
      | SListAccess(e1, e2) ->
            let arr = build_expr builder env e1 in
            let idx = build_expr builder env e2 in
            let p = L.build_gep arr [|idx|] "tmp" builder in
            L.build_load p "tmp" builder
            (* TODO: list index out of bounds *)
      | SMutateList((e1, i), e2) as ex ->
            let arr = build_expr builder env e1 in
            let idx = build_expr builder env i in
            let value = build_expr builder env e2 in
            let p = L.build_gep arr [|idx; L.const_int i32_t 0|] "tmp" builder in
            L.build_store p value builder
            (* TODO: list index out of bounds *)
      | SCall ("echoi", [e]) -> L.build_call printf_func [| int_format_str ; (build_expr builder env e) |] "printf" builder
      | SCall ("echof", [e]) -> L.build_call printf_func [| flt_format_str ; (build_expr builder env e) |] "printf" builder
			| SCall ("echo", [e]) -> L.build_call printf_func [| str_format_str ; (build_expr builder env e) |] "printf" builder
      | SCall ("bash", [e]) -> L.build_call bash_func [| (build_expr builder env e) |] "fork_exec" builder
			| SCall ("sqrt", [e]) -> L.build_call sqrt_func [| (build_expr builder env e) |] "josh_sqrt" builder
			| SCall ("fsqrt", [e]) -> L.build_call fsqrt_func [| (build_expr builder env e) |] "josh_sqrt2" builder
			| SCall ("int_to_float", [e]) -> L.build_call itf_func [| (build_expr builder env e) |] "int_to_float" builder
			| SCall ("float_to_int", [e]) -> L.build_call fti_func [| (build_expr builder env e) |] "float_to_int" builder
      | SCall ("pow", args) -> 
        let llargs = List.rev (List.map (build_expr builder env) (List.rev args)) in
        L.build_call pow_func (Array.of_list llargs) "josh_pow" builder
      | SCall ("fpow", args) -> 
        let llargs = List.rev (List.map (build_expr builder env) (List.rev args)) in
        L.build_call fpow_func (Array.of_list llargs) "josh_pow2" builder
      | SCall ("concat", args) -> 
        let llargs = List.rev (List.map (build_expr builder env) (List.rev args)) in
        L.build_call concat_func (Array.of_list llargs) "josh_concat" builder
      | SCall ("subset", args) -> 
        let llargs = List.rev (List.map (build_expr builder env) (List.rev args)) in
        L.build_call subset_func (Array.of_list llargs) "josh_subset" builder
      | SCall ("strcmp", args) -> 
        let llargs = List.rev (List.map (build_expr builder env) (List.rev args)) in
        L.build_call strcmp_func (Array.of_list llargs) "josh_strcmp" builder
			| SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder env) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder

    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    let rec build_stmt builder ((frame::frames) as env) = function
        SBlock sl -> List.fold_left (fun (a,b) x -> build_stmt a b x) (builder,env) sl
      | SExpr e -> ignore(build_expr builder env e); (builder, env)
      | SReturn e -> ignore(L.build_ret (build_expr builder env e) builder); (builder,env)
      (*| SContinue -> ignore(L.build_ret (build_expr builder env e) builder); (builder,env) (* TODO *) *)
      (*| SBreak -> ignore(L.build_ret (build_expr builder env e) builder); (builder,env)  (* TODO *) *)
      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder env predicate in

        let body_bb = L.append_block context "while_body" the_function in
        let (builder', _) = build_stmt (L.builder_at_end context body_bb) (StringMap.empty::env) body in
        add_terminal builder' build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        (L.builder_at_end context end_bb, env)
      | SFor (e1, e2, e3, body) -> build_stmt builder env
           (SBlock [SExpr e1; SWhile (e2, SBlock [body; SExpr e3])])
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder env predicate in

        let then_bb = L.append_block context "then" the_function in
        let (then_builder,_) = build_stmt (L.builder_at_end context then_bb) (StringMap.empty::env) then_stmt in
        let else_bb = L.append_block context "else" the_function in
        let (else_builder,_) = build_stmt (L.builder_at_end context else_bb) (StringMap.empty::env) else_stmt in

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal then_builder build_br_end;
        add_terminal else_builder build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        (L.builder_at_end context end_bb, env)


      | SVdecl svdecl -> (match svdecl with
        | SDeclare(t, id) ->
            let frame' = add_local frame (t, id) in
            (builder, frame'::frames)
        | SInitialize (t, id, expr) ->
            let frame' = add_local frame (t, id) in
            ignore(build_expr builder (frame'::frames) (t, SAssign(id, expr)));
            (builder, frame'::frames)
        | _ -> (builder,env)
      )
      | _ -> (builder,env)
    in

    let (func_builder, _) = build_stmt builder [formals; global_vars] (SBlock fdecl.sbody) in
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in

  let build_record_formals_ltypes_array (opt_list : A.opt list) =
    let rec aux (opt_list : A.opt list) llopt_list = match opt_list with
    | [] -> llopt_list
    | A.Opt(typ, _)::tl -> aux tl ( (ltype_of_typ typ [||])::llopt_list)
    in
    let formals_ltypes_list = List.rev (aux opt_list []) in
    let find_ith_type i = (List.nth formals_ltypes_list i) in
    Array.init (List.length formals_ltypes_list) find_ith_type
  in

  List.iter (fun x ->
        let (SRecordDef(rec_name, formals)) = x in
        let record = (L.named_struct_type context rec_name) in
        ignore(L.struct_set_body record (build_record_formals_ltypes_array formals) false);
        ignore(L.declare_global record rec_name josh_module);
    ) record_defs;

  List.iter build_function_body functions;

  let llmem = L.MemoryBuffer.of_file "liberate_josh.bc" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  ignore(Llvm_linker.link_modules' josh_module llm);
  josh_module
