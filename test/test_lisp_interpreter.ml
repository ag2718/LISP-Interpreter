open Core
open Lisp_interpreter

let examples_dir = "examples"

let get_scm_files () =
  Stdlib.Sys.readdir examples_dir
  |> Array.to_list
  |> List.filter ~f:(fun f -> String.is_suffix f ~suffix:".scm")
  |> List.sort ~compare:String.compare

let run_interpreter input =
  let out_file = Stdlib.Filename.temp_file "lisp_test" ".out" in
  let out_chan = Out_channel.create out_file in

  let in_file = Stdlib.Filename.temp_file "lisp_test" ".in" in
  Out_channel.write_all in_file ~data:input;
  let in_chan = In_channel.create in_file in

  Repl.repl_loop in_chan out_chan 0 "" Env.empty;

  In_channel.close in_chan;
  Out_channel.close out_chan;
  let output = In_channel.read_all out_file in

  Stdlib.Sys.remove in_file;
  Stdlib.Sys.remove out_file;

  output

let normalize output =
  output |> String.split_lines |> List.map ~f:String.rstrip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:"\n"

let run_test scm_file =
  let base_name = String.chop_suffix_exn scm_file ~suffix:".scm" in
  let scm_path = Filename.concat examples_dir scm_file in
  let out_path = Filename.concat examples_dir (base_name ^ ".out") in

  let input = In_channel.read_all scm_path in
  let expected = In_channel.read_all out_path in

  try
    let actual = run_interpreter input in
    let expected_normalized = normalize expected in
    let actual_normalized = normalize actual in

    if String.equal expected_normalized actual_normalized then (
      printf "✓ %s\n" base_name;
      true)
    else (
      printf "✗ %s\n" base_name;
      printf "  Expected:\n";
      expected_normalized |> String.split_lines
      |> List.iter ~f:(fun line -> printf "    %s\n" line);
      printf "  Got:\n";
      actual_normalized |> String.split_lines
      |> List.iter ~f:(fun line -> printf "    %s\n" line);
      false)
  with exn ->
    printf "✗ %s (exception: %s)\n" base_name (Exn.to_string exn);
    false

let () =
  printf "Running Lisp Interpreter Tests\n";
  printf "==============================\n\n";

  let scm_files = get_scm_files () in
  let results = List.map scm_files ~f:run_test in

  let passed = List.count results ~f:Fn.id in
  let total = List.length results in

  printf "\n==============================\n";
  printf "Results: %d/%d tests passed\n" passed total;

  if passed < total then exit 1 else exit 0
