open Core

exception Unreachable

let map (f : 'a -> 'b) p =
  {
    parse =
      (fun inp ->
        match p.parse inp with
        | Ok (a, inp') -> Ok (f a, inp')
        | Error error -> Error error);
  }

let peek p =
  {
    parse =
      (fun inp ->
        match p.parse inp with Ok (a, _) -> Ok (a, inp) | Error e -> Error e);
  }

let many_as_string p = p |> many |> map (fun a -> String.of_seq (List.to_seq a))
let not_char ch = sat (fun x -> x != ch)

let backslash =
  {
    parse =
      (fun inp ->
        match (char' '\\').parse inp with
        | Ok (_, inp') -> (
            match item.parse inp' with
            | Ok ('"', inp'') -> Ok ('"', inp'')
            | Ok ('n', inp'') -> Ok ('\n', inp'')
            | Ok ('t', inp'') -> Ok ('\t', inp'')
            | Ok _ ->
                Error { err = "invalid character after \\"; pos = inp'.pos }
            | Error e -> Error e)
        | Error e -> Error e);
  }

let literal =
  char' '"' *> many_as_string (backslash <|> not_char '"') <* char' '"'

let fold_decimal l =
  let v, _ =
    List.fold_right
      (fun v (acc, decimal) -> (acc + (decimal * v), decimal * 10))
      l (0, 1)
  in
  v

let integer =
  bind
    (fun (l, pos) ->
      if List.length l > 0 then result' l
      else zero { err = "expected integer"; pos })
    (many (digit |> map (fun ch -> Char.code ch - Char.code '0')))
  |> map fold_decimal

let boolean =
  string' "true" <|> string' "false"
  |> map (function "true" -> true | "false" -> false | _ -> raise Unreachable)
