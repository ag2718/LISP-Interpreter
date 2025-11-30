(** value.mli - Core types for the interpreter *)

type id = string

type env = {
  parent : env option;
  mutable bindings : (string, value, Core.String.comparator_witness) Core.Map.t;
}

and value =
  | Val_nil
  | Val_bool of bool
  | Val_int of int
  | Val_prim of (value list -> value)
  | Val_lambda of env * id list * Ast.expr list

val empty_env : env
val to_string : value -> string
val expect_int : value -> int
val expect_bool : value -> bool

