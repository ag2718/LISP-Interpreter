let init () =
  let _ = Arithmetic.registered in
  let _ = Comparison.registered in
  let _ = Boolean.registered in
  ()

let () = init ()

module Arithmetic = Arithmetic
module Comparison = Comparison
module Boolean = Boolean
