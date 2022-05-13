module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate decls = 
  (* boilerplate *)
  let context = L.global_context() in
  let josh_module = L.create_module context "Josh" in

  (* types *)
  let i32_t       = L.i32_type    context
  and i8_t        = L.i8_type     context
  and i1_t        = L.i1_type     context 
  and float_type  = L.float_type  context
  and void_type   = L.void_type context
  (*and array_type  = (L.array_type)*)
  and record_type = (L.struct_type context) in

  let ltype_of_typ typ arr = match typ with
      A.Int           -> i32_t
    | A.Bool          -> i1_t
    | A.Char          -> i8_t
    | A.Float         -> float_type
    | A.Void          -> void_type
    | A.String        -> L.pointer_type (L.i8_type context)
    | A.RecordType id -> record_type arr
    (*| A.ListT l -> array_type*)
    | _ -> i32_t
  in

  (* Get all functions declarations in source *)
  let functions = List.filter_map
                  (function SFdecl f -> Some f | _ -> None)
                  decls
  in

  (* TODO: get top level vdecls *)
  let record_defs = List.filter_map (function
                                      | SStmt(sstmt) -> (match sstmt with
                                        | SRecordDef(id, opts_list) -> Some (SRecordDef(id, opts_list))
                                        | _ -> None)
                                      | _ -> None) decls
  in

  let globals = []
  in

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t [||]) 0
      in StringMap.add n (L.define_global n init josh_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t josh_module in

	let bash_t : L.lltype = 
    L.function_type i32_t [| (L.pointer_type (L.i8_type context)) |] in
	let bash_func : L.llvalue = 
		L.declare_function "fork_exec" bash_t josh_module in

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
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in


    (* create space for each function paramter *)
    let add_formal m (t, n) p =
      L.set_value_name n p;

      (* TODO: expand to support records/lists *)
      let local = L.build_alloca (ltype_of_typ t [||]) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m
    in

    let formals = List.fold_left2 add_formal StringMap.empty (List.map (fun (A.Opt(t, id)) -> (t,id)) fdecl.sformals)
        (Array.to_list (L.params the_function)) in

    let add_local m (t, n) =
      (* create space for each local var declaration *)
        let local_var = match t with
        | A.RecordType id ->
            let global_type = (L.type_by_name josh_module "Person") in (match global_type with 
              Some t -> L.build_alloca t n builder
              | None -> raise(Failure("Unrecognized record type")))
        | _ -> L.build_alloca (ltype_of_typ t [||]) n builder
        in StringMap.add n local_var m
      in

    (* TODO: FIX *)
    let rec lookup n = function
        [] -> raise (Failure ("Not found: " ^ n))
      | (frame::frames) ->
        try StringMap.find n frame
        with Not_found -> lookup n frames
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

    let rec build_expr builder ((frame::frames) as env) ((_, e) : sexpr) = match e with
        SIntLit i                      -> L.const_int i32_t i 
      | SRecordCreate (id, sexpr_list) -> let global_type = (L.type_by_name josh_module "Person") in (match global_type with 
                                          Some t -> (L.build_alloca t id builder) (*L.build_alloca (record_type (build_record_ltypes_array sexpr_list)) id builder*)
                                          | None -> raise(Failure("Unrecognized record type")))

      | SBoolLit b                     -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s                      -> L.build_global_stringptr s "tmp" builder
      | SId id                         -> L.build_load (lookup id env) id builder
      | SAssign (s, e)                 -> let e' = build_expr builder env e in
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
      | SCall ("echoi", [e]) -> L.build_call printf_func [| int_format_str ; (build_expr builder env e) |] "printf" builder
      | SCall ("echo", [e]) -> L.build_call printf_func [| str_format_str ; (build_expr builder env e) |] "printf" builder
      | SCall ("bash", [e]) -> L.build_call bash_func [| (build_expr builder env e) |] "fork_exec" builder
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
        add_terminal then_builder (*(L.builder_at_end context then_bb)*) build_br_end;
        add_terminal else_builder (*(L.builder_at_end context else_bb)*) build_br_end;

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
        let record = (L.named_struct_type context "Person") in 
        ignore(L.struct_set_body record (build_record_formals_ltypes_array formals) false);
        ignore(L.declare_global record "Person" josh_module);
    ) record_defs;

  List.iter build_function_body functions;

	let llmem = L.MemoryBuffer.of_file "liberate_josh.bc" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
	ignore(Llvm_linker.link_modules' josh_module llm);
  josh_module

