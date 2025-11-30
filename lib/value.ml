(** value.ml - Core types for the interpreter *)

open Core

type id = string

type env = {
  parent : env option;
  mutable bindings : (string, value, String.comparator_witness) Map.t;
}

and value =
  | Val_nil
  | Val_bool of bool
  | Val_int of int
  | Val_prim of (value list -> value)
  | Val_lambda of env * id list * Ast.expr list

let empty_env : env = { parent = None; bindings = Map.empty (module String) }

let to_string : value -> string = function
  | Val_nil -> "nil"
  | Val_bool b -> if b then "true" else "false"
  | Val_int i -> Int.to_string i
  | Val_prim _ -> "<primitive>"
  | Val_lambda _ -> "<lambda>"

let expect_int : value -> int = function
  | Val_int i -> i
  | v -> failwithf "expected int, got %s" (to_string v) ()

let expect_bool : value -> bool = function
  | Val_bool b -> b
  | v -> failwithf "expected bool, got %s" (to_string v) ()
