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

let empty : env = { parent = None; bindings = Map.empty (module String) }

let to_string = function
  | Val_nil -> "nil"
  | Val_bool b -> if b then "true" else "false"
  | Val_int i -> Int.to_string i
  | Val_prim _ -> "<primitive>"
  | Val_lambda _ -> "<lambda>"

let builtin_map =
  let expect_int : value -> int = function
    | Val_int i -> i
    | v -> failwithf "expected int, got %s" (to_string v) ()
  in

  let expect_bool : value -> bool = function
    | Val_bool b -> b
    | v -> failwithf "expected bool, got %s" (to_string v) ()
  in
  Map.of_alist_exn
    (module String)
    [
      ( "+",
        fun args ->
          Val_int (List.fold args ~init:0 ~f:(fun acc v -> acc + expect_int v))
      );
      ( "*",
        fun args ->
          Val_int (List.fold args ~init:1 ~f:(fun acc v -> acc * expect_int v))
      );
      ( "-",
        function
        | [ x ] -> Val_int (-expect_int x)
        | [ x; y ] -> Val_int (expect_int x - expect_int y)
        | args ->
            failwithf "(-) expects 1 or 2 arguments, got %d" (List.length args)
              () );
      ( "/",
        function
        | [ x; y ] -> Val_int (expect_int x / expect_int y)
        | args ->
            failwithf "(/) expects 2 arguments, got %d" (List.length args) () );
      ( "MAX",
        function
        | [] -> failwith "MAX expects at least 1 argument"
        | args ->
            Val_int
              (List.fold args ~init:Int.min_value ~f:(fun acc v ->
                   Int.max acc (expect_int v))) );
      ( "ABS",
        function
        | [ x ] -> Val_int (Int.abs (expect_int x))
        | args ->
            failwithf "ABS expects 1 argument, got %d" (List.length args) () );
      ( "=",
        function
        | [ x; y ] -> Val_bool (expect_int x = expect_int y)
        | args ->
            failwithf "(=) expects 2 arguments, got %d" (List.length args) () );
      ( "!",
        function
        | [ x ] -> Val_bool (not (expect_bool x))
        | args ->
            failwithf "(!) expects 1 argument, got %d" (List.length args) () );
      ( ">",
        function
        | [ x; y ] -> Val_bool (expect_int x > expect_int y)
        | args ->
            failwithf "(>) expects 2 arguments, got %d" (List.length args) () );
      ( ">=",
        function
        | [ x; y ] -> Val_bool (expect_int x >= expect_int y)
        | args ->
            failwithf "(>=) expects 2 arguments, got %d" (List.length args) ()
      );
      ( "<",
        function
        | [ x; y ] -> Val_bool (expect_int x < expect_int y)
        | args ->
            failwithf "(<) expects 2 arguments, got %d" (List.length args) () );
      ( "<=",
        function
        | [ x; y ] -> Val_bool (expect_int x <= expect_int y)
        | args ->
            failwithf "(<=) expects 2 arguments, got %d" (List.length args) ()
      );
    ]

let rec eval_expr env expr =
  match expr with
  | Ast.Expr_nil -> (Val_nil, env)
  | Ast.Expr_bool b -> (Val_bool b, env)
  | Ast.Expr_int i -> (Val_int i, env)
  | Ast.Expr_id id ->
      let is_builtin id = Map.mem builtin_map id in
      let get_builtin id = Val_prim (Map.find_exn builtin_map id) in

      let rec lookup_id env id =
        match Map.find env.bindings id with
        | Some v -> v
        | None -> (
            match env.parent with
            | Some parent_env -> lookup_id parent_env id
            | None -> failwithf "identifier '%s' not found in environment" id ()
            )
      in
      if is_builtin id then (get_builtin id, env) else (lookup_id env id, env)
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
  | Ast.Expr_if (cond, then_cl, else_cl) -> (
      match eval_expr env cond with
      | Val_bool true, _ -> eval_expr env then_cl
      | Val_bool false, _ -> eval_expr env else_cl
      | other, _ ->
          failwithf "if condition must be bool, got: %s" (to_string other) ())
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
      | v -> failwithf "cannot apply non-function: %s" (to_string v) ())
