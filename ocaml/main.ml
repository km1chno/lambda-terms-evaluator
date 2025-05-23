open Parser
open Common
open Evaluator

let verbose = ref false
let filename = ref ""

let speclist =
  [
    ("--v", Arg.Set verbose, "Enable verbose mode");
    ("-f", Arg.Set_string filename, "Input file name");
  ]

let usage_msg = "Usage: lcevaluator [options]"

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

(* Main function reading a term on input and parsing it *)
let () =
  Arg.parse speclist print_endline usage_msg;
  let input = read_file !filename in
  let output =
    match parse_term input with
    | Some term ->
        if !verbose then Printf.printf "Parsed term: %s\n" (show_term term);
        term_to_string (evaluate term)
    | _ -> "parsing error"
  in
  Printf.printf "%s\n" output
