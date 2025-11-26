open Lisp_interpreter

let () =
  Printf.printf "--- Scheme REPL ---\n> ";
  Repl.repl_loop In_channel.stdin Out_channel.stdout 0 "" Env.empty
