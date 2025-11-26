open Core

let count_char (c : char) (s : string) : int = String.count s ~f:(Char.equal c)

let strip_comment (line : string) : string =
  match String.lsplit2 line ~on:';' with
  | Some (before, _) -> before
  | None -> line

let rec repl_loop (in_chan : In_channel.t) (out_chan : Out_channel.t)
    (open_count : int) (buffer : string) (env : Env.env) : unit =
  Out_channel.flush out_chan;

  match In_channel.input_line in_chan with
  | Some line -> (
      let line_wo_comments = strip_comment line in
      let new_open_count =
        open_count
        + count_char '(' line_wo_comments
        - count_char ')' line_wo_comments
      in
      let updated_buf = buffer ^ "\n" ^ line_wo_comments in
      match Int.sign new_open_count with
      | Neg -> printf "Invalid s-expr!"
      | Pos -> repl_loop in_chan out_chan new_open_count updated_buf env
      | Zero ->
          let trimmed = String.strip updated_buf in
          if String.length trimmed > 0 then (
            let result, new_env =
              trimmed |> Sexp.of_string |> Ast.of_sexp |> Env.eval_expr env
            in
            let result_str = Env.to_string result in
            fprintf out_chan "%s\n" result_str;
            if phys_equal out_chan Out_channel.stdout then printf "> ";
            repl_loop in_chan out_chan 0 "" new_env)
          else repl_loop in_chan out_chan 0 "" env)
  | None ->
      if phys_equal out_chan Out_channel.stdout then printf "Ending session."
