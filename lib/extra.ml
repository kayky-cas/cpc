open Core

let map (f : 'a -> 'b) p =
  {
    parse =
      (fun inp ->
        match p.parse inp with Ok (a, inp') -> Ok (f a, inp') | error -> error);
  }
