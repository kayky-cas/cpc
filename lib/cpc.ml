type error = { err : string; pos : int }
type input = { text : string; pos : int }
type 'a parser = { parse : input -> ('a * input, error) result }

let input s = { text = s; pos = 0 }
let result' a = { parse = (fun inp -> Ok (a, inp)) }
let zero = { parse = (fun _ -> Error { err = "unexpected error"; pos = -1 }) }

let item =
  {
    parse =
      (fun inp ->
        match String.to_seq inp.text () with
        | Seq.Nil -> Error { err = "expected char"; pos = inp.pos }
        | Seq.Cons (ch, tail) ->
            Ok (ch, { text = String.of_seq tail; pos = inp.pos + 1 }));
  }

let seq (p1 : 'a parser) (p2 : 'b parser) =
  {
    parse =
      (fun inp ->
        match p1.parse inp with
        | Ok (a, inp') -> (
            match p2.parse inp' with
            | Ok (b, inp'') -> Ok ((a, b), inp'')
            | Error e -> Error e)
        | Error e -> Error e);
  }

let ( <*> ) = seq

let ( *> ) (p1 : 'a parser) (p2 : 'b parser) =
  {
    parse =
      (fun inp ->
        match p1.parse inp with Ok (_, inp') -> p2.parse inp' | err -> err);
  }

let ( <* ) (p1 : 'a parser) (p2 : 'b parser) =
  {
    parse =
      (fun inp ->
        match p1.parse inp with
        | Ok (a, inp') -> (
            match p2.parse inp' with
            | Ok (_, inp'') -> Ok (a, inp'')
            | err -> err)
        | err -> err);
  }

let bind (f : 'a -> 'b parser) p =
  {
    parse =
      (fun inp ->
        match p.parse inp with
        | Ok (a, inp') -> (f a).parse inp'
        | Error e -> Error e);
  }

let sat (f : char -> bool) =
  bind (fun x -> if f x then result' x else zero) item

let char' ch = sat (fun x -> x == ch)
let digit = sat (function '0' .. '9' -> true | _ -> false)
let lower = sat (function 'a' .. 'z' -> true | _ -> false)
let upper = sat (function 'A' .. 'Z' -> true | _ -> false)

let plus p1 p2 =
  {
    parse =
      (fun inp ->
        match p1.parse inp with
        | Error { err; pos } -> (
            match p2.parse inp with
            | Error { err = err'; _ } ->
                Error { err = Printf.sprintf "%s or %s" err err'; pos }
            | parser' -> parser')
        | parser' -> parser');
  }

let ( <|> ) = plus
let letter = lower <|> upper
let alphanum = letter <|> digit

let map (f : 'a -> 'b) p =
  {
    parse =
      (fun inp ->
        match p.parse inp with Ok (a, inp') -> Ok (f a, inp') | error -> error);
  }
