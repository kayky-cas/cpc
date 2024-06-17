type error = { err : string; pos : int }
type input = { text : string; pos : int }
type 'a parser = input -> ('a * input, error) result

module Util = struct
  let explode s = List.init (String.length s) (String.get s)
end

let input s = { text = s; pos = 0 }
let result' a : 'a parser = fun inp -> Ok (a, inp)
let zero e : 'a parser = fun _ -> Error e

let item inp =
  match String.to_seq inp.text () with
  | Seq.Nil -> Error { err = "expected char"; pos = inp.pos }
  | Seq.Cons (ch, tail) ->
      Ok (ch, { text = String.of_seq tail; pos = inp.pos + 1 })
