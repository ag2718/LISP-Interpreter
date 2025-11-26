open Base
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
  | Val_prim of (value list -> value) (* primitive functions *)
  | Val_lambda of env * id list * Ast.expr list

let empty = { parent = None; bindings = Map.empty (module String) }

let is_builtin = function
  | "+" -> true
  | "-" -> true
  | "/" -> true
  | "*" -> true
  | "MAX" -> true
  | "ABS" -> true
  | "=" -> true
  | "!" -> true
  | ">" -> true
  | ">=" -> true
  | "<" -> true
  | "<=" -> true
  | _ -> false

let builtin_funcs func =
  let rec extract_ints acc = function
    | [] -> List.rev acc
    | hd :: tl -> (
        match hd with
        | Val_int i -> extract_ints (i :: acc) tl
        | _ -> failwith "non-int value where int value expected!")
  in
  let rec extract_bools acc = function
    | [] -> List.rev acc
    | hd :: tl -> (
        match hd with
        | Val_bool b -> extract_bools (b :: acc) tl
        | _ -> failwith "non-bool value where bool value expected!")
  in
  match func with
  | "+" ->
      Val_prim
        (fun args ->
          Val_int (List.fold (extract_ints [] args) ~init:0 ~f:( + )))
  | "*" ->
      Val_prim
        (fun args ->
          Val_int (List.fold (extract_ints [] args) ~init:1 ~f:( * )))
  | "-" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 1 -> Val_int (-List.hd_exn int_args)
          | 2 -> Val_int (List.nth_exn int_args 0 - List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (-)")
  | "/" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 2 -> Val_int (List.nth_exn int_args 0 / List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (/)")
  | "MAX" ->
      Val_prim
        (fun args ->
          Val_int
            (List.fold (extract_ints [] args) ~init:0 ~f:(fun acc x ->
                 if acc > x then acc else x)))
  | "ABS" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 1 -> Val_int (int_args |> List.hd_exn |> abs)
          | _ -> failwith "invalid number of arguments to ABS")
  | "=" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 2 -> Val_bool (List.nth_exn int_args 0 = List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (=)")
  | "!" ->
      Val_prim
        (fun args ->
          let bool_args = extract_bools [] args in
          match List.length bool_args with
          | 1 -> Val_bool (not (List.hd_exn bool_args))
          | _ -> failwith "invalid number of arguments to (!)")
  | ">" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 2 -> Val_bool (List.nth_exn int_args 0 > List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (>)")
  | ">=" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 2 -> Val_bool (List.nth_exn int_args 0 >= List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (>=)")
  | "<" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 2 -> Val_bool (List.nth_exn int_args 0 < List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (<)")
  | "<=" ->
      Val_prim
        (fun args ->
          let int_args = extract_ints [] args in
          match List.length int_args with
          | 2 -> Val_bool (List.nth_exn int_args 0 <= List.nth_exn int_args 1)
          | _ -> failwith "invalid number of arguments to (<=)")
  | _ -> assert false

let rec eval_expr env = function
  | Ast.Expr_nil -> (Val_nil, env)
  | Ast.Expr_bool b -> (Val_bool b, env)
  | Ast.Expr_int i -> (Val_int i, env)
  | Ast.Expr_id id ->
      if is_builtin id then (builtin_funcs id, env)
      else
        let rec lookup_id env id =
          match Map.find env.bindings id with
          | Some v -> v
          | None -> (
              match env.parent with
              | Some parent_env -> lookup_id parent_env id
              | None ->
                  failwith (Printf.sprintf "identifier %s not found in env!" id)
              )
        in
        (lookup_id env id, env)
  | Ast.Expr_def (id, expr) -> (
      match expr with
      | Expr_lambda (args, exprs) ->
          let env_ref = ref { parent = env.parent; bindings = env.bindings } in
          let lambda_val = Val_lambda (!env_ref, args, exprs) in
          !env_ref.bindings <-
            Map.set !env_ref.bindings ~key:id ~data:lambda_val;
          (Val_nil, !env_ref)
      | _ ->
          let value, _ = eval_expr env expr in
          let updated_env =
            {
              parent = env.parent;
              bindings = Map.set env.bindings ~key:id ~data:value;
            }
          in
          (Val_nil, updated_env))
  | Ast.Expr_let (ids, val_exprs, body_exprs) ->
      let values = List.map val_exprs ~f:(fun e -> fst (eval_expr env e)) in
      let new_bindings =
        List.fold2_exn ids values ~init:env.bindings
          ~f:(fun bindings id value -> Map.set bindings ~key:id ~data:value)
      in
      let let_env = { parent = env.parent; bindings = new_bindings } in
      let result, _ =
        List.fold_left body_exprs ~init:(Val_nil, let_env)
          ~f:(fun (_, e) expr -> eval_expr e expr)
      in
      (result, env)
  | Ast.Expr_if (cond, then_cl, else_cl) ->
      let cond_val = eval_expr env cond in
      let eval_cl =
        match cond_val with
        | Val_bool true, _ -> then_cl
        | Val_bool false, _ -> else_cl
        | _ -> failwith "if condition does not evaluate to boolean!"
      in
      eval_expr env eval_cl
  | Ast.Expr_lambda (args, exprs) -> (Val_lambda (env, args, exprs), env)
  | Ast.Expr_apply (func, args) -> (
      let func_val = fst (eval_expr env func) in
      let arg_vals = List.map args ~f:(fun x -> fst (eval_expr env x)) in
      match func_val with
      | Val_prim builtin_fn -> (builtin_fn arg_vals, env)
      | Val_lambda (captured_env, params, exprs) ->
          let bindings =
            List.fold2_exn params arg_vals
              ~init:(Map.empty (module String))
              ~f:(fun bindings param v -> Map.set bindings ~key:param ~data:v)
          in
          let call_env = { parent = Some captured_env; bindings } in
          let res, _ =
            List.fold_left exprs ~init:(Val_nil, call_env)
              ~f:(fun (_, env_acc) expr -> eval_expr env_acc expr)
          in
          (res, env)
      | _ -> assert false)

let to_string = function
  | Val_nil -> "nil"
  | Val_bool b -> if b then "true" else "false"
  | Val_int i -> Int.to_string i
  | Val_prim _ -> "<primitive>"
  | Val_lambda _ -> "<lambda>"
