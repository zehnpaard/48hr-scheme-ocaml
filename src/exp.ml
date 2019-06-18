module E = Expenv
type t = E.expt =
  | Atom of string
  | List of E.expt list
  | DottedList of E.expt list * E.expt
  | Number of int
  | String of string
  | Bool of bool
  | PrimitiveFunc of (E.expt list -> E.expt)
  | Func of E.fn
  | PortIn of in_channel
  | PortOut of out_channel

type fn = E.fn =
  {params : string list;
   varargs : string option;
   body : E.expt list;
   closure : E.envt}

let rec to_string = function
  | Atom a -> a
  | List es ->
      let s = List.map to_string es |> String.concat " " in
      Printf.sprintf "(%s)" s
  | DottedList (es, e) ->
      let s = List.map to_string es |> String.concat " " in
      Printf.sprintf "(%s . %s)" s (to_string e)
  | Number n -> string_of_int n
  | String s -> "\"" ^ s ^ "\""
  | Bool b -> string_of_bool b
  | PrimitiveFunc _ -> "<primitive>"
  | Func f ->
      let paramss = String.concat " " f.params in
      let varargss = (match f.varargs with
        | Some s -> " . " ^ s
        | None -> "")
      in
      Printf.sprintf "(lambda (%s%s) ...)" paramss varargss
  | PortIn _ -> "<IO Input Port>"
  | PortOut _ -> "<IO Output Port>"
