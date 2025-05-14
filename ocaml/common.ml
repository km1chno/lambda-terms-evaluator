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

let rec term_to_string (t : term) : string =
  match t with
  | Abs (x, t) -> "\\" ^ String.make 1 x ^ "." ^ term_to_string t
  | App (s, t) -> "(" ^ term_to_string s ^ ")(" ^ term_to_string t ^ ")"
  | Var x -> String.make 1 x
