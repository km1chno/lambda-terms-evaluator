(* A subset of traditional Alternative implementation for parser. *)
module Parser = struct
  type 'a parser = char list -> ('a * char list) option

  let ( <$> ) (f : 'a -> 'b) (p : 'a parser) : 'b parser =
   fun s -> p s |> Option.map (fun (v, s') -> (f v, s'))

  let ( <*> ) (p1 : ('a -> 'b) parser) (p2 : 'a parser) : 'b parser =
   fun s ->
    match p1 s with
    | None -> None
    | Some (f, s') -> (
        match p2 s' with None -> None | Some (x, s'') -> Some (f x, s''))

  let ( *> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
   fun s ->
    match p1 s with
    | None -> None
    | Some (_, s') -> (
        match p2 s' with None -> None | Some (x, s'') -> Some (x, s''))

  let ( <* ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
   fun s ->
    match p1 s with
    | None -> None
    | Some (x, s') -> (
        match p2 s' with None -> None | Some (_, s'') -> Some (x, s''))

  let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
   fun s -> match p1 s with None -> p2 s | some -> some
end

(* Parsers for lambda terms CFG *)
open Parser
open Common

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

(* Helping fuction to run the parser *)
let parse_term (s : string) : term option =
  match term_parser (List.of_seq (String.to_seq s)) with
  | Some (term, []) -> Some term
  | _ -> None
