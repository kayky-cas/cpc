open Core

let map (f : 'a -> 'b) p =
  {
    parse =
      (fun inp ->
        match p.parse inp with
        | Ok (a, inp') -> Ok (f a, inp')
        | Error error -> Error error);
  }

let many_as_string (p : 'a parser) =
  p |> many |> map (fun a -> String.of_seq (List.to_seq a))

let not_char ch = sat (fun x -> x != ch)
let literal = char' '"' *> many_as_string (not_char '"') <* char' '"'

let fold_decimal l =
  let v, _ =
    List.fold_right
      (fun v (acc, decimal) -> (acc + (decimal * v), decimal * 10))
      l (0, 1)
  in
  v

let integer =
  many (digit |> map (fun ch -> Char.code ch - Char.code '0'))
  |> map fold_decimal

let boolean = string' "true" <|> string' "false"
