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
  | [badarg] -> raise @@ Exception.TypeMismatch ("pair", Exp.to_string badarg)
  | badargs ->
      let n = List.length badargs in
      raise @@ Exception.NumArgs (1, string_of_int n ^ " args")

let cdr = function
  | [List (_::xs)] -> List xs
  | [DottedList ([_], y)] -> y
  | [DottedList (_::xs, y)] -> DottedList (xs, y)
  | [badarg] -> raise @@ Exception.TypeMismatch ("pair", Exp.to_string badarg)
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
  |> P.add "car" car
  |> P.add "cdr" cdr
  |> P.add "cons" cons
  |> P.add "eq?" eqv
  |> P.add "eqv?" eqv
  |> P.add "equal?" equal


let apply func args = match P.find_opt func primitives with
  | None -> raise @@ Exception.NotFunction ("Unrecognized primitive function", func)
  | Some f -> f args

let rec f env e = match e with
  | String _ -> e
  | Number _ -> e
  | Bool _ -> e
  | Atom x -> Env.get_var env x
  | List [Atom "quote"; e'] -> e'
  | List [Atom "if"; e1; e2; e3] ->
      (match f env e1 with
        | Bool false -> f env e3
        | _ -> f env e2)
  | List [Atom "set!"; Atom v; e'] ->
      f env e' |> Env.set_var env v
  | List [Atom "define"; Atom v; e'] ->
      f env e' |> Env.define_var env v
  | List (Atom func :: args) ->
      List.map (f env) args
      |> apply func
  | bad -> raise @@ Exception.BadSpecialForm ("Unrecognized form", Exp.to_string bad)
