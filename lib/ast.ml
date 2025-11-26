(** ast.ml *)

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

let rec of_sexp : Sexp.t -> expr = function
  | Sexp.Atom "nil" -> Expr_nil
  | Sexp.Atom "#t" -> Expr_bool true
  | Sexp.Atom "#f" -> Expr_bool false
  | Sexp.Atom s -> (
      match int_of_string_opt s with Some i -> Expr_int i | None -> Expr_id s)
  | Sexp.List l -> (
      match l with
      | [] -> assert false
      | [ Sexp.Atom "define"; Sexp.Atom id; value ] ->
          Expr_def (id, of_sexp value)
      | Sexp.Atom "let" :: Sexp.List binding_exprs :: stmts ->
          let get_binding = function
            | Sexp.List [ Sexp.Atom id; value ] -> (id, of_sexp value)
            | sexp ->
                failwithf "invalid let binding: %s" (Sexp.to_string_hum sexp) ()
          in
          let bindings = List.map binding_exprs ~f:get_binding in
          let ids = List.map bindings ~f:fst in
          let exprs = List.map bindings ~f:snd in
          Expr_let (ids, exprs, List.map stmts ~f:of_sexp)
      | [ Sexp.Atom "if"; cond; then_cl; else_cl ] ->
          Expr_if (of_sexp cond, of_sexp then_cl, of_sexp else_cl)
      | Sexp.Atom "lambda" :: Sexp.List args :: body ->
          let args_list =
            List.map args ~f:(function
              | Sexp.Atom id -> id
              | sexp ->
                  failwithf "invalid lambda argument: %s"
                    (Sexp.to_string_hum sexp) ())
          in
          let body_exprs = List.map body ~f:of_sexp in
          Expr_lambda (args_list, body_exprs)
      | hd :: tl ->
          let tl_exprs = List.map tl ~f:of_sexp in
          Expr_apply (of_sexp hd, tl_exprs))

let to_string : expr -> string =
  let indent n = String.make (n * 2) ' ' in
  let rec is_complex = function
    | Expr_lambda _ | Expr_def _ | Expr_if _ -> true
    | Expr_apply (_, args) -> List.exists args ~f:is_complex
    | _ -> false
  in
  let rec aux depth = function
    | Expr_nil -> "NIL"
    | Expr_bool t -> if t then "TRUE" else "FALSE"
    | Expr_int n -> Int.to_string n
    | Expr_id id -> id
    | Expr_def (id, expr) ->
        let i1 = indent (depth + 1) in
        sprintf "DEFINE %s\n%s%s" id i1 (aux (depth + 1) expr)
    | Expr_let (ids, binding_exprs, body_exprs) ->
        let i1 = indent (depth + 1) in
        let bindings_str =
          List.map2_exn ids binding_exprs ~f:(fun id expr ->
              sprintf "(%s %s)" id (aux (depth + 1) expr))
          |> String.concat ~sep:" "
        in
        let body_strs = List.map body_exprs ~f:(aux (depth + 1)) in
        let body = String.concat body_strs ~sep:("\n" ^ i1) in
        sprintf "LET (%s)\n%s%s" bindings_str i1 body
    | Expr_if (cond, then_cl, else_cl) ->
        let i1 = indent (depth + 1) in
        sprintf "IF %s\n%sTHEN: %s\n%sELSE: %s"
          (aux (depth + 1) cond)
          i1
          (aux (depth + 1) then_cl)
          i1
          (aux (depth + 1) else_cl)
    | Expr_lambda (args, exprs) ->
        let i1 = indent (depth + 1) in
        let args_str = String.concat args ~sep:" " in
        let body_strs = List.map exprs ~f:(aux (depth + 1)) in
        let body = String.concat body_strs ~sep:("\n" ^ i1) in
        sprintf "LAMBDA(%s)\n%s%s" args_str i1 body
    | Expr_apply (func, args) ->
        let func_str = aux depth func in
        if List.is_empty args then sprintf "(%s)" func_str
        else if List.exists args ~f:is_complex then
          let i1 = indent (depth + 1) in
          let args_strs = List.map args ~f:(aux (depth + 1)) in
          let args_str = String.concat args_strs ~sep:("\n" ^ i1) in
          sprintf "(%s\n%s%s)" func_str i1 args_str
        else
          let args_strs = List.map args ~f:(aux (depth + 1)) in
          let args_str = String.concat args_strs ~sep:" " in
          sprintf "(%s %s)" func_str args_str
  in
  aux 0
