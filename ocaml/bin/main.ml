open Parser

(* The Term algebraic data type *)
type term = Abs of char * term | App of term * term | Var of char

(* Auxiliary curryfied term constructors *)
let build_var (c : char) : term = Var c
let build_app (s : term) (t : term) : term = App (s, t)
let build_abs (x : char) (t : term) : term = Abs (x, t)

(* Auxiliary a'la builtin functions *)
let is_lowercase = function 'a' .. 'z' -> true | _ -> false

let rec show_term (t : term) : string =
  match t with
  | Abs (x, t) -> "Abs " ^ String.make 1 x ^ " [" ^ show_term t ^ "]"
  | App (s, t) -> "App [" ^ show_term s ^ "] [" ^ show_term t ^ "]"
  | Var x -> "Var " ^ String.make 1 x

(* Parsers for lambda terms CFG *)
open Parser

let char_parser (b : char) : char parser =
 fun s -> match s with c :: rest when c == b -> Some (c, rest) | _ -> None

let variable_parser : char parser =
 fun s ->
  match s with c :: rest when is_lowercase c -> Some (c, rest) | _ -> None

let rec term_parser : term parser =
 fun s ->
  let termInBrackets = char_parser '(' *> term_parser <* char_parser ')' in
  let application =
    build_app
    <$> (char_parser '(' *> term_parser <* char_parser ')')
    <*> (char_parser '(' *> term_parser <* char_parser ')')
  in
  let abstraction =
    build_abs
    <$> (char_parser '\\' *> variable_parser <* char_parser '.')
    <*> term_parser
  in
  let variable = build_var <$> variable_parser in
  (abstraction <|> application <|> termInBrackets <|> variable) s

(* Main function reading a term on input and parsing it *)
let () =
  let input = read_line () in
  let parsing_result = term_parser (List.of_seq (String.to_seq input)) in
  let output =
    match parsing_result with
    | Some (term, _) -> show_term term
    | _ -> "parsing error"
  in
  Printf.printf "%s\n" output
