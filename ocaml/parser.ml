(* A subset of traditional Alternative implementation for parser. *)
module Parser = struct
  type 'a parser = string -> ('a * string) option

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

  let rec many (p : 'a parser) : 'a list parser =
   fun s ->
    match p s with
    | None -> Some ([], s)
    | Some (x, s') -> (
        match many p s' with
        | None -> Some ([ x ], s')
        | Some (ys, s'') -> Some (x :: ys, s''))

  let many1 (p : 'a parser) : 'a list parser =
   fun s ->
    match p s with
    | None -> None
    | Some (x, s') -> (
        match many p s' with
        | None -> Some ([ x ], s')
        | Some (ys, s'') -> Some (x :: ys, s''))
end

(* Parsers for lambda terms CFG *)
open Parser
open Common
open String

let list_to_string (seq : char list) : string = String.of_seq (List.to_seq seq)

let is_alphanumeric (c : char) =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')

let alphanumeric_parser : char parser =
 fun s ->
  match List.of_seq (String.to_seq s) with
  | c :: rest when is_alphanumeric c -> Some (c, list_to_string rest)
  | _ -> None

let seq_parser (z : string) : string parser =
 fun s ->
  if starts_with ~prefix:z s then
    Some (z, sub s (length z) (length s - length z))
  else None

let variable_parser : string parser =
  list_to_string <$> many1 alphanumeric_parser

let rec term_parser : term parser =
 fun s ->
  let termInBrackets = seq_parser "(" *> term_parser <* seq_parser ")" in
  let application =
    build_app
    <$> (seq_parser "(" *> term_parser <* seq_parser ")")
    <*> (seq_parser "(" *> term_parser <* seq_parser ")")
  in
  let abstraction =
    build_abs
    <$> (seq_parser "λ" *> variable_parser <* seq_parser ".")
    <*> term_parser
  in
  let variable = build_var <$> variable_parser in
  (abstraction <|> application <|> termInBrackets <|> variable) s

(* Helping fuction to run the parser *)
let parse_term (s : string) : term option =
  match term_parser s with
  | Some (term, rest) when length rest == 0 -> Some term
  | _ -> None
