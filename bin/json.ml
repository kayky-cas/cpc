open Cpc

exception Foo

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let content = really_input_string ic n in
  close_in ic;
  content

let () =
  (match
     Array.get Sys.argv 1 |> read_file |> Core.input |> Json.value.parse
   with
  | Ok (a, _) -> a
  | Error _ -> raise Foo)
  |> Json.show_value_t |> print_endline
