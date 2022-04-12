module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate decls = 
  (* boilerplate *)
  let context = L.global_context() in
  let josh_module = L.create_module context "Josh" in

  (* types *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and float_type = L.float_type  context
  and void_type = L.void_type context
  and array_type = (L.array_type)
  and struct_type = (L.struct_type context) in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char  -> i8_t
    | A.Float -> float_type
    | A.Void -> void_type
    (*| A.RecordType r -> struct_type 
    | A.ListT l -> array_type*)
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
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init josh_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* create llvm function declarations using the function declarations from source *)
  let function_decls : (L.llvalue * sfdecl) StringMap.t =
    let function_decl m fdecl = 
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) (List.map (fun (A.Opt(t,id)) -> (t,id)) fdecl.sformals))
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype josh_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
    
  (* For each llvm function declaration, create the corresponding llvm 3-addr code function body *)
  let build_function_body fdecl = 
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let local_vars =

      (* create space for each function paramter *)
      let add_formal m (t, n) p =
        L.set_value_name n p;

        (* TODO: expand to support records/lists *)
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
        
      (* create space for each local var declaration *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      (* formals is a map of params to their LLVM allocated space of appropriate size *)
      let vdecls_in_function = 
        List.filter_map 
        (function 
          | SVdecl svdecl -> match svdecl with
                SDeclare(t,id) -> Some (t,id) 
              | SInitialize(t,id, s) -> Some (t,id) (* ? *)
              | _ -> None
          | _ -> None)
        fdecl.sbody in

      let formals = List.fold_left2 add_formal StringMap.empty (List.map (fun (A.Opt(t, id)) -> (t,id)) fdecl.sformals)
        (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals vdecls_in_function
    in

    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let rec build_expr builder ((_, e) : sexpr) = match e with
        SIntLit i -> L.const_int i32_t i 
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'

    in

    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | _ -> builder
    in

    let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in

  List.iter build_function_body functions;
  josh_module

