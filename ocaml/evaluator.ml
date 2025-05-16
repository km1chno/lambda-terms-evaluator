open Common

(* Perform substitution t[x <- v] *)
let rec substitute (t : term) (x : string) (v : term) : term =
  match t with
  | App (s, t) -> App (substitute s x v, substitute t x v)
  | Abs (y, s) -> Abs (y, substitute s x v)
  | Var y when String.equal x y -> v
  | Var _ -> t
  | Macro (name, s, t) -> Macro (name, substitute s x v, substitute t x v)

let rec evaluate (s : term) : term =
  match s with
  | App (u, v) -> (
      match (evaluate u, evaluate v) with
      | Abs (x, t), v -> evaluate (substitute t x v)
      | u, v -> App (u, v))
  | Abs (x, t) -> Abs (x, evaluate t)
  | Var x -> Var x
  | Macro (name, s, t) -> evaluate (substitute t name (evaluate s))
