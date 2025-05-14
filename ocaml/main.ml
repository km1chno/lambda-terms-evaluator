open Parser
open Common

(* Main function reading a term on input and parsing it *)
let () =
  let input = read_line () in
  let output =
    match parse_term input with
    | Some term -> show_term term
    | _ -> "parsing error"
  in
  Printf.printf "%s\n" output
