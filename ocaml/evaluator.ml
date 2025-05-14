open Common

(* Perform substitution u[x <- v] *)
let rec substitute (u : term) (x : char) (v : term) : term =
  match u with
  | App (s, t) -> App (substitute s x v, substitute t x v)
  | Abs (y, s) -> Abs (y, substitute s x v)
  | Var y when x == y -> v
  | _ -> u

let rec evaluate (v : term) : term =
  match v with
  | App (s, t) -> (
      match s with
      | Abs (x, u) -> evaluate (substitute u x t)
      | _ -> App (evaluate s, evaluate t))
  | Abs (x, s) -> Abs (x, evaluate s)
  | Var x -> Var x
