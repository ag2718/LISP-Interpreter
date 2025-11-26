open Core

let rec repl_loop in_chan out_chan open_count buffer env =
  Out_channel.flush out_chan;

  match In_channel.input_line in_chan with
  | Some line -> (
      let line_wo_comments = List.hd_exn (String.split line ~on:';') in
      Out_channel.flush Out_channel.stdout;
      let count_occurrences char str =
        String.fold str ~init:0 ~f:(fun cnt c ->
            if Char.equal c char then cnt + 1 else cnt)
      in
      let new_open_count =
        open_count
        + count_occurrences '(' line_wo_comments
        - count_occurrences ')' line_wo_comments
      in
      let updated_buf = buffer ^ "\n" ^ line_wo_comments in
      match new_open_count with
      | n when n < 0 -> Printf.printf "Invalid s-expr!"
      | n when n > 0 ->
          repl_loop in_chan out_chan new_open_count updated_buf env
      | _ ->
          let trimmed = String.strip updated_buf in
          if String.length trimmed > 0 then (
            let result, new_env =
              trimmed |> Sexp.of_string |> Ast.of_sexp |> Env.eval_expr env
            in
            let result_str = Env.to_string result in
            Printf.fprintf out_chan "%s\n" result_str;
            if phys_equal out_chan Out_channel.stdout then
              Printf.fprintf out_chan "> ";
            repl_loop in_chan out_chan 0 "" new_env)
          else repl_loop in_chan out_chan 0 "" env)
  | None ->
      if phys_equal out_chan Out_channel.stdout then
        Printf.printf "Ending session."
