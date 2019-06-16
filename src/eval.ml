open Exp

let apply func args = match func with
  | PrimitiveFunc f -> f args
  | _ -> raise @@ Exception.NotFunction ("Non-function found at front position", to_string func)

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
  | List (head :: tail) ->
      let func = f env head in
      let args = List.map (f env) tail in
      apply func args
  | bad -> raise @@ Exception.BadSpecialForm ("Unrecognized form", Exp.to_string bad)
