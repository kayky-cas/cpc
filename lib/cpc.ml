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

let ( <> ) = seq

let bind p (f : 'a -> 'b parser) =
  {
    parse =
      (fun inp ->
        match p.parse inp with
        | Ok (a, inp') -> (f a).parse inp'
        | Error e -> Error e);
  }
