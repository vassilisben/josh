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
