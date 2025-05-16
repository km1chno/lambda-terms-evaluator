open Parser
open Common
open Evaluator

let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (acc ^ line ^ "\n")
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines ""

let is_whitespace (c : char) : bool =
  c == ' ' || c == '\n' || c == '\t' || c == '\r'

let clean_whitespaces (s : string) : string =
  String.of_seq (Seq.filter (fun c -> not (is_whitespace c)) (String.to_seq s))

(* Main function reading a term on input and parsing it *)
let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let input = clean_whitespaces (read_file filename) in
    let output =
      match parse_term input with
      | Some term -> term_to_string (evaluate term)
      | _ -> "parsing error"
    in
    Printf.printf "%s\n" output
