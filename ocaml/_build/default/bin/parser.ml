module Parser = struct
  type 'a parser = unit -> char list -> ('a * char list) option

  let ( <$> ) (f : 'a -> 'b) (p : 'a parser) : 'b parser =
   fun _ -> fun s -> p () s |> Option.map (fun (v, s') -> (f v, s'))

  let ( <*> ) (p1 : ('a -> 'b) parser) (p2 : 'a parser) : 'b parser =
   fun _ ->
    fun s ->
     match p1 () s with
     | None -> None
     | Some (f, s') -> (
         match p2 () s' with None -> None | Some (x, s'') -> Some (f x, s''))

  let ( *> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
   fun _ ->
    fun s ->
     match p1 () s with
     | None -> None
     | Some (_, s') -> (
         match p2 () s' with None -> None | Some (x, s'') -> Some (x, s''))

  let ( <* ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
   fun _ ->
    fun s ->
     match p1 () s with
     | None -> None
     | Some (x, s') -> (
         match p2 () s' with None -> None | Some (_, s'') -> Some (x, s''))

  let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
   fun _ -> fun s -> match p1 () s with None -> p2 () s | some -> some
end
