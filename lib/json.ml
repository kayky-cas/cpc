open Core
open Extra

type value_t =
  | Str of string
  | Number of float
  | Object of (string * value_t) list
  | List of value_t list
  | Boolean of bool

let pair_to_float (a, b) =
  let a_float = float_of_int a in
  let b_float = float_of_int b in
  let b_scaled =
    b_float /. (10.0 ** float_of_int (String.length (string_of_int b)))
  in
  a_float +. b_scaled

let number =
  integer
  <*> (char' '.' *> integer <|> result' 0)
  |> map (fun x -> Number (pair_to_float x))

let literal = literal |> map (fun s -> Str s)
let boolean = boolean |> map (fun b -> Boolean b)
let space = many (char' ' ' <|> char' '\n' <|> char' '\t')

let value : value_t parser =
  literal <|> number <|> (* object' <|> list' <|> *) boolean

let entry = Extra.literal <* space <* char' '=' <* space <*> value

let list' =
  char' '[' *> many (space *> value <* space <* char' ',' <* space) <* char' ']'
