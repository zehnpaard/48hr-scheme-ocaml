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


let car = function
  | [List (x::_)] -> x
  | [DottedList (x::_, _)] -> x
  | [badarg] -> 
      raise @@ Exception.TypeMismatch (
          "car expects non-empty pair",
          Exp.to_string badarg)
  | badargs ->
      let n = List.length badargs in
      raise @@ Exception.NumArgs (1, string_of_int n ^ " args")

let cdr = function
  | [List (_::xs)] -> List xs
  | [DottedList ([_], y)] -> y
  | [DottedList (_::xs, y)] -> DottedList (xs, y)
  | [badarg] ->
      raise @@ Exception.TypeMismatch (
          "cdr expects non-empty pair",
          Exp.to_string badarg)
  | badargs ->
      let n = List.length badargs in
      raise @@ Exception.NumArgs (1, string_of_int n ^ " args")

let cons = function
  | [x; List xs] -> List (x :: xs)
  | [x; DottedList (xs, y)] -> DottedList (x :: xs, y)
  | [x; y] -> DottedList ([x], y)
  | badargs ->
      let n = List.length badargs in
      raise @@ Exception.NumArgs (2, string_of_int n ^ " args")

let rec eqv' = function
  | [Atom x; Atom y] -> x = y
  | [Number x; Number y] -> x = y
  | [String x; String y] -> x = y
  | [Bool x; Bool y] -> x = y
  | [DottedList (xs, x); DottedList (ys, y)] ->
      eqv' [List (x::xs); List (y::ys)]
  | [List xs; List ys] ->
      List.length xs = List.length ys && eqv_list xs ys
  | [_; _] -> false
  | badargs ->
      let n = List.length badargs in
      raise @@ Exception.NumArgs (2, string_of_int n ^ " args")
and eqv_list xs ys = match xs, ys with
  | [], [] -> true
  | x::xs', y::ys' -> if eqv' [x; y] then eqv_list xs' ys' else false
  | _ -> false

let eqv x = Bool (eqv' x)

let equal =
  let comp op x y =
    try op x = op y
    with Exception.TypeMismatch _ -> false
  in
  function
    | [x; y] as arg ->
        Bool (eqv' arg || comp to_int x y || comp to_str x y || comp to_bool x y)
    | badargs ->
        let n = List.length badargs in
        raise @@ Exception.NumArgs (2, string_of_int n ^ " args")

let load env =
  let add v e env = (Env.define_var env v (PrimitiveFunc e) |> ignore; env) in
  env
  |> add "+" (numeric_binop (+))
  |> add "-" (numeric_binop (-))
  |> add "*" (numeric_binop ( * ))
  |> add "/" (numeric_binop (/))
  |> add "mod" (numeric_binop (mod))
  |> add "=" (num_bool_binop (=))
  |> add "<" (num_bool_binop (<))
  |> add ">" (num_bool_binop (>))
  |> add "/=" (num_bool_binop (!=))
  |> add "<=" (num_bool_binop (<=))
  |> add ">=" (num_bool_binop (>=))
  |> add "&&" (bool_bool_binop (&&))
  |> add "||" (bool_bool_binop (||))
  |> add "string=?" (str_bool_binop (=))
  |> add "string<?" (str_bool_binop (<))
  |> add "string>?" (str_bool_binop (>))
  |> add "string/=?" (str_bool_binop (!=))
  |> add "string<=?" (str_bool_binop (<=))
  |> add "string>=?" (str_bool_binop (>=))
  |> add "car" car
  |> add "cdr" cdr
  |> add "cons" cons
  |> add "eq?" eqv
  |> add "eqv?" eqv
  |> add "equal?" equal
  |> add "open-input-file" Io.make_port_in
  |> add "open-output-file" Io.make_port_out
  |> add "close-input-file" Io.close_port_in
  |> add "close-output-file" Io.close_port_out
  |> add "read" Io.read_proc
  |> add "write" Io.write_proc
  |> add "read-contents" Io.read_contents
  |> add "read-all" Io.read_all
