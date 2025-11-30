open Core
open Lisp_interpreter.Value

let registered = ()

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "+"
      let arity = None
      let doc = "Add numbers. (+ a b c ...) => a + b + c + ..."

      let apply args =
        Val_int (List.fold args ~init:0 ~f:(fun acc v -> acc + expect_int v))
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "*"
      let arity = None
      let doc = "Multiply numbers. (* a b c ...) => a * b * c * ..."

      let apply args =
        Val_int (List.fold args ~init:1 ~f:(fun acc v -> acc * expect_int v))
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "-"
      let arity = None
      let doc = "Subtract. (- a) => -a, (- a b) => a - b"

      let apply = function
        | [ x ] -> Val_int (-expect_int x)
        | [ x; y ] -> Val_int (expect_int x - expect_int y)
        | args ->
            failwithf "(-) expects 1 or 2 arguments, got %d" (List.length args)
              ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "/"
      let arity = Some 2
      let doc = "Divide. (/ a b) => a / b (integer division)"

      let apply = function
        | [ x; y ] -> Val_int (expect_int x / expect_int y)
        | args ->
            failwithf "(/) expects 2 arguments, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "MAX"
      let arity = None
      let doc = "Maximum of arguments. (MAX a b c ...) => max(a, b, c, ...)"

      let apply = function
        | [] -> failwith "MAX expects at least 1 argument"
        | args ->
            Val_int
              (List.fold args ~init:Int.min_value ~f:(fun acc v ->
                   Int.max acc (expect_int v)))
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "MIN"
      let arity = None
      let doc = "Minimum of arguments. (MIN a b c ...) => min(a, b, c, ...)"

      let apply = function
        | [] -> failwith "MIN expects at least 1 argument"
        | args ->
            Val_int
              (List.fold args ~init:Int.max_value ~f:(fun acc v ->
                   Int.min acc (expect_int v)))
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "ABS"
      let arity = Some 1
      let doc = "Absolute value. (ABS n) => |n|"

      let apply = function
        | [ x ] -> Val_int (Int.abs (expect_int x))
        | args ->
            failwithf "ABS expects 1 argument, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "MOD"
      let arity = Some 2
      let doc = "Modulo. (MOD a b) => a mod b"

      let apply = function
        | [ x; y ] -> Val_int (expect_int x % expect_int y)
        | args ->
            failwithf "MOD expects 2 arguments, got %d" (List.length args) ()
    end)
