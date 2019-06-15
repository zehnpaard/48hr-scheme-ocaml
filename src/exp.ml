type t = Atom of string
       | List of t list
       | DottedList of t list * t
       | Number of int
       | String of string
       | Bool of bool

let rec to_string = function
  | Atom a -> a
  | List es ->
      let s = List.map to_string es |> String.concat " " in
      Printf.sprintf "(%s)" s
  | DottedList (es, e) ->
      let s = List.map to_string es |> String.concat " " in
      Printf.sprintf "(%s . %s)" s (to_string e)
  | Number n -> string_of_int n
  | String s -> s
  | Bool b -> string_of_bool b
