open Core
open Lisp_interpreter.Value

let registered = ()

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "="
      let arity = Some 2
      let doc = "Equality. (= a b) => #t if a equals b"

      let apply = function
        | [ x; y ] -> Val_bool (expect_int x = expect_int y)
        | args ->
            failwithf "(=) expects 2 arguments, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "<"
      let arity = Some 2
      let doc = "Less than. (< a b) => #t if a < b"

      let apply = function
        | [ x; y ] -> Val_bool (expect_int x < expect_int y)
        | args ->
            failwithf "(<) expects 2 arguments, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "<="
      let arity = Some 2
      let doc = "Less than or equal. (<= a b) => #t if a <= b"

      let apply = function
        | [ x; y ] -> Val_bool (expect_int x <= expect_int y)
        | args ->
            failwithf "(<=) expects 2 arguments, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = ">"
      let arity = Some 2
      let doc = "Greater than. (> a b) => #t if a > b"

      let apply = function
        | [ x; y ] -> Val_bool (expect_int x > expect_int y)
        | args ->
            failwithf "(>) expects 2 arguments, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = ">="
      let arity = Some 2
      let doc = "Greater than or equal. (>= a b) => #t if a >= b"

      let apply = function
        | [ x; y ] -> Val_bool (expect_int x >= expect_int y)
        | args ->
            failwithf "(>=) expects 2 arguments, got %d" (List.length args) ()
    end)
