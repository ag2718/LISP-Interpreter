open Core
open Lisp_interpreter.Value

let registered = ()

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "!"
      let arity = Some 1
      let doc = "Logical not. (! b) => #t if b is #f, #f otherwise"

      let apply = function
        | [ x ] -> Val_bool (not (expect_bool x))
        | args ->
            failwithf "(!) expects 1 argument, got %d" (List.length args) ()
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "and"
      let arity = None
      let doc = "Logical and. (and a b c ...) => #t if all are #t"
      let apply args = Val_bool (List.for_all args ~f:(fun v -> expect_bool v))
    end)

let () =
  Lisp_interpreter.Registry.register
    (module struct
      let name = "or"
      let arity = None
      let doc = "Logical or. (or a b c ...) => #t if any is #t"
      let apply args = Val_bool (List.exists args ~f:(fun v -> expect_bool v))
    end)
