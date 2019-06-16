open Exp

let rec to_int = function
  | Number n -> n
  | String s ->
      (try int_of_string s
       with Failure _ -> raise @@ Exception.TypeMismatch ("number", s))
  | List [e] -> to_int e
  | e -> raise @@ Exception.TypeMismatch ("number", Exp.to_string e)

let to_str = function
  | String s -> s
  | Number n -> string_of_int n
  | Bool b -> string_of_bool b
  | e -> raise @@ Exception.TypeMismatch ("string", Exp.to_string e)

let to_bool = function
  | Bool b -> b
  | e -> raise @@ Exception.TypeMismatch ("bool", Exp.to_string e)

let numeric_binop op params =
  let n = match List.map to_int params with
    | [] -> raise @@ Exception.NumArgs (2, "No args")
    | [_] -> raise @@ Exception.NumArgs (2, "One arg")
    | x :: xs -> List.fold_left op x xs
  in
  Number n

let bool_binop f op = function
  | [x; y] -> Bool (op (f x) (f y))
  | args ->
      let n = List.length args in
      raise @@ Exception.NumArgs (2, string_of_int n ^ " args")

let num_bool_binop = bool_binop to_int
let str_bool_binop = bool_binop to_str
let bool_bool_binop = bool_binop to_bool

module P = Map.Make(String)
let primitives = P.empty
  |> P.add "+" (numeric_binop (+))
  |> P.add "-" (numeric_binop (-))
  |> P.add "*" (numeric_binop ( * ))
  |> P.add "/" (numeric_binop (/))
  |> P.add "mod" (numeric_binop (mod))
  |> P.add "=" (num_bool_binop (=))
  |> P.add "<" (num_bool_binop (<))
  |> P.add ">" (num_bool_binop (>))
  |> P.add "/=" (num_bool_binop (!=))
  |> P.add "<=" (num_bool_binop (<=))
  |> P.add ">=" (num_bool_binop (>=))
  |> P.add "&&" (bool_bool_binop (&&))
  |> P.add "||" (bool_bool_binop (||))
  |> P.add "string=?" (str_bool_binop (=))
  |> P.add "string<?" (str_bool_binop (<))
  |> P.add "string>?" (str_bool_binop (>))
  |> P.add "string/=?" (str_bool_binop (!=))
  |> P.add "string<=?" (str_bool_binop (<=))
  |> P.add "string>=?" (str_bool_binop (>=))

let apply func args = match P.find_opt func primitives with
  | None -> raise @@ Exception.NotFunction ("Unrecognized primitive function", func)
  | Some f -> f args

let rec f e = match e with
  | String _ -> e
  | Number _ -> e
  | Bool _ -> e
  | List [Atom "quote"; e2] -> e2
  | List [Atom "if"; e1, e2, e3] ->
      (match f e1 with
        | Bool false -> eval e3
        | _ -> eval e2)
  | List (Atom func :: args) ->
      List.map f args
      |> apply func
  | bad -> raise @@ Exception.BadSpecialForm ("Unrecognized form", Exp.to_string bad)
