open Lisp_interpreter

(* Force all builtins to load by calling Builtins.init *)
let () = Builtins.init ()

let () =
  Printf.printf "--- Scheme REPL ---\n> ";
  Repl.repl_loop In_channel.stdin Out_channel.stdout 0 "" Env.empty
