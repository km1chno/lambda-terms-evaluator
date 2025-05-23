(* The Term algebraic data type *)
type term =
  | Abs of string * term
  | App of term * term
  | Var of string
  | Macro of string * term * term

(* Auxiliary curryfied term constructors *)
let build_var (c : string) : term = Var c
let build_app (s : term) (t : term) : term = App (s, t)
let build_abs (x : string) (t : term) : term = Abs (x, t)
let build_macro (name : string) (t : term) (s : term) = Macro (name, t, s)

let build_app_chain (l : term list) : term =
  match l with
  | [ s; t ] -> build_app s t
  | s :: t :: rest -> List.fold_left build_app (build_app s t) rest
  | _ -> raise (Invalid_argument "Cannot build application chain from one term")

(* Auxiliary a'la builtin functions *)
let list_to_string (seq : char list) : string = String.of_seq (List.to_seq seq)

let is_alphanumeric (c : char) =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')

let is_whitespace (c : char) : bool =
  c == ' ' || c == '\n' || c == '\t' || c == '\r'

let is_lowercase = function 'a' .. 'z' -> true | _ -> false

let rec show_term (t : term) : string =
  match t with
  | Abs (x, t) -> "Abs " ^ x ^ " [" ^ show_term t ^ "]"
  | App (s, t) -> "App [" ^ show_term s ^ "] [" ^ show_term t ^ "]"
  | Var x -> "Var " ^ x
  | Macro (name, s, t) ->
      "Macro " ^ name ^ " [" ^ show_term s ^ "] [" ^ show_term t ^ "]"

let rec term_to_string (t : term) : string =
  match t with
  | Abs (x, t) -> "λ" ^ x ^ "." ^ term_to_string t
  | App (s, t) -> (
      match (s, t) with
      | Var x, Var y -> x ^ " " ^ y
      | Var x, _ -> x ^ "(" ^ term_to_string t ^ ")"
      | _, Var y -> "(" ^ term_to_string s ^ ")" ^ y
      | _ -> "(" ^ term_to_string s ^ ")(" ^ term_to_string t ^ ")")
  | Var x -> x
  | Macro (name, s, t) ->
      "let " ^ name ^ " = " ^ term_to_string s ^ ";;\n" ^ term_to_string t
