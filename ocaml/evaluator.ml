open Common

(* Perform substitution t[x <- v] *)
let rec substitute (t : term) (x : string) (v : term) : term =
  match t with
  | App (s, t) -> App (substitute s x v, substitute t x v)
  | Abs (y, s) -> Abs (y, substitute s x v)
  | Var y when String.equal x y -> v
  | _ -> t

let rec evaluate (s : term) : term =
  match s with
  | App (u, v) -> (
      match u with
      | Abs (x, t) -> evaluate (substitute t x v)
      | _ -> App (evaluate u, evaluate v))
  | Abs (x, t) -> Abs (x, evaluate t)
  | Var x -> Var x
