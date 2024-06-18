type error = { err : string; pos : int }
type input = { text : string; pos : int }
type 'a parser = { parse : input -> ('a * input, error) result }

let input s = { text = s; pos = 0 }
let result' a = { parse = (fun inp -> Ok (a, inp)) }
let zero e = { parse = (fun _ -> Error e) }

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

let ( *> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  {
    parse =
      (fun inp ->
        match p1.parse inp with
        | Ok (_, inp') -> p2.parse inp'
        | Error err -> Error err);
  }

let ( <* ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  {
    parse =
      (fun inp ->
        match p1.parse inp with
        | Ok (a, inp') -> (
            match p2.parse inp' with
            | Ok (_, inp'') -> Ok (a, inp'')
            | Error err -> Error err)
        | Error err -> Error err);
  }

let bind (f : 'a * int -> 'b parser) p =
  {
    parse =
      (fun inp ->
        match p.parse inp with
        | Ok (a, inp') -> (f (a, inp.pos)).parse inp'
        | Error e -> Error e);
  }

let sat (f : char -> bool) =
  bind
    (fun (x, pos) ->
      if f x then result' x
      else zero { err = Printf.sprintf "invalid char at %d" pos; pos })
    item

let char' ch = sat (fun x -> x == ch)
let digit = sat (function '0' .. '9' -> true | _ -> false)
let lower = sat (function 'a' .. 'z' -> true | _ -> false)
let upper = sat (function 'A' .. 'Z' -> true | _ -> false)

let plus (p1 : 'a parser) (p2 : 'b parser) =
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

let many p =
  {
    parse =
      (fun inp ->
        let l = ref [] in
        let i = ref inp in
        let quit_loop = ref false in

        while (not !quit_loop) && String.length !i.text > 0 do
          match p.parse !i with
          | Ok (a, inp') ->
              l := a :: !l;
              i := inp'
          | Error _ -> quit_loop := true
        done;

        Ok (List.rev !l, !i));
  }

let rec string' s : string parser =
  match String.to_seq s () with
  | Seq.Nil -> result' ""
  | Seq.Cons (ch, tail) ->
      bind
        (fun _ ->
          bind
            (fun _ -> result' (String.of_seq (Seq.cons ch tail)))
            (string' (String.of_seq tail)))
        (char' ch)
