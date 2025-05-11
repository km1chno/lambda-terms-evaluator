open Parser

(* The Term algebraic data type *)
type term = Abs of char * term | App of term * term | Var of char

(* Auxiliary curryfied term constructors *)
let build_var (c : char) : term = Var c
let build_app (s : term) (t : term) : term = App (s, t)
let build_abs (x : char) (t : term) : term = Abs (x, t)

let rec show_term (t : term) : string =
  match t with
  | Abs (x, t) -> "Abs " ^ String.make 1 x ^ " [" ^ show_term t ^ "]"
  | App (s, t) -> "App [" ^ show_term s ^ "] [" ^ show_term t ^ "]"
  | Var x -> "Var " ^ String.make 1 x

open Parser

let char_parser (b : char) : char parser =
 fun _ ->
  fun s -> match s with c :: rest when c == b -> Some (c, rest) | _ -> None

let variable_parser : char parser =
 fun _ ->
  fun s ->
   match s with
   | c :: rest when Char.lowercase_ascii c == c -> Some (c, rest)
   | _ -> None

let rec term_parser_l () : term parser =
  char_parser '(' *> term_parser_l ()
  <* char_parser ')'
  <|> (build_app
      <$> (char_parser '(' *> term_parser_l () <* char_parser ')')
      <*> (char_parser '(' *> term_parser_l () <* char_parser ')'))
  <|> (build_abs
      <$> (char_parser '\\' *> variable_parser <* char_parser '.')
      <*> term_parser_l ())
  <|> (build_var <$> variable_parser)

let () =
  let parser = term_parser_l () in
  Printf.printf "%s\n" "Finished!"
(* let parsing_result = *)
(*   term_parser_l () () (List.of_seq (String.to_seq "\\a.a")) *)
(* in *)
(* let output = *)
(*   match parsing_result with *)
(*   | Some (term, _) -> show_term term *)
(*   | _ -> "parsing error" *)
(* in *)
(* Printf.printf "%s\n" output *)
