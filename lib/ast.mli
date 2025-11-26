(** ast.mli *)

open Core

type id = string

type expr =
  | Expr_nil
  | Expr_bool of bool
  | Expr_int of int
  | Expr_id of id
  | Expr_def of id * expr
  | Expr_let of id list * expr list * expr list
  | Expr_if of expr * expr * expr
  | Expr_lambda of id list * expr list
  | Expr_apply of expr * expr list

val of_sexp : Sexp.t -> expr
val to_string : expr -> string
