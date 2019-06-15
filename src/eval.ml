open Exp

let rec to_int = function
  | Number n -> n
  | String s -> (try int_of_string s with Failure _ -> 0)
  | List [e] -> to_int e
  | _ -> 0

let numeric_binop op params =
  let n = match List.map to_int params with
    | [] -> 0
    | x :: xs -> List.fold_left op x xs
  in
  Number n

module P = Map.Make(String)
let primitives = P.empty
  |> P.add "+" (numeric_binop (+))
  |> P.add "-" (numeric_binop (-))
  |> P.add "*" (numeric_binop ( * ))
  |> P.add "/" (numeric_binop (/))
  |> P.add "mod" (numeric_binop (mod))

let apply func args = match P.find_opt func primitives with
  | None -> Bool false
  | Some f -> f args

let rec f e = match e with
  | String _ -> e
  | Number _ -> e
  | Bool _ -> e
  | List [Atom "quote"; e2] -> e2
  | List (Atom func :: args) ->
      List.map f args
      |> apply func
  | _ -> failwith "Evaluation not implemented"
