open Parser
open Common
open Evaluator

(* Main function reading a term on input and parsing it *)
let () =
  let input = read_line () in
  let output =
    match parse_term input with
    | Some term -> term_to_string (evaluate term)
    | _ -> "parsing error"
  in
  Printf.printf "%s\n" output
