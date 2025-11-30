(** env.mli *)

type id = Value.id
type env = Value.env

type value = Value.value =
  | Val_nil
  | Val_bool of bool
  | Val_int of int
  | Val_prim of (value list -> value)
  | Val_lambda of env * id list * Ast.expr list

val empty : env
val eval_expr : env -> Ast.expr -> value * env
val to_string : value -> string
