module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate decls = 
  (* boilerplate *)
  let context = L.global_context() in
  let josh_module = L.create_module context "Josh" in

  let stmts = List.filter_map (function SStmt s -> Some s | _ -> None) decls in


  (* types *)
  let i32_t       = L.i32_type    context
  and i8_t        = L.i8_type     context
  and i1_t        = L.i1_type     context 
  and float_type  = L.float_type  context
  and void_type   = L.void_type context
  and string_type = L.pointer_type (L.i8_type context)
  (*and array_type  = (L.array_type)*)
  and record_type = (L.struct_type context) in

  let ltype_of_typ typ arr = match typ with
      A.Int           -> i32_t
    | A.Bool          -> i1_t
    | A.Char          -> i8_t
    | A.Float         -> float_type
    | A.Void          -> void_type
    | A.String        -> string_type 
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

    let local_vars =
      (* create space for each function paramter *)
      let add_formal m (t, n) p =
        L.set_value_name n p;

        (* TODO: expand to support records/lists *)
        let local = L.build_alloca (ltype_of_typ t [||]) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
        
      (* create space for each local var declaration *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t [||]) n builder
        in StringMap.add n local_var m
      in

      (* formals is a map of params to their LLVM allocated space of appropriate size *)
      let vdecls_in_function = 
        List.filter_map 
        (fun stmt -> match stmt with
          | SVdecl svdecl -> (match svdecl with
            SDeclare (t,id) -> Some (t,id)
            | _ -> None
          )
          | _ -> None)
        fdecl.sbody in

      let formals = List.fold_left2 add_formal StringMap.empty (List.map (fun (A.Opt(t, id)) -> (t,id)) fdecl.sformals)
        (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals vdecls_in_function
    in

    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
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

    let rec build_expr builder ((_, e) : sexpr) = match e with
        SIntLit i                      -> L.const_int i32_t i 
      | SRecordCreate (id, sexpr_list) -> L.build_alloca (record_type (build_record_ltypes_array sexpr_list)) id builder
      | SBoolLit b                     -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s                      -> L.build_global_stringptr s "tmp" builder
      | SId id                         -> L.build_load (lookup id) id builder
      | SAssign (s, e)                 -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
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
      | SCall ("echoi", [e]) -> L.build_call printf_func [| int_format_str ; (build_expr builder e) |] "printf" builder
      | SCall ("echo", [e]) -> L.build_call printf_func [| str_format_str ; (build_expr builder e) |] "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in 
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder

    in

    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | _ -> builder
    in

    let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in

  List.iter build_function_body functions;
  josh_module

