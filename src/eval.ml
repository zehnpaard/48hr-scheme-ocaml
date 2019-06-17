open Exp

let rec bind_args' params args env = match params, args with
  | [], [] -> env
  | p::params', a::args' -> (Env.define_var env p a |> ignore; bind_args' params' args' env)
  | _ -> raise @@ Exception.Default "Interpreter error - failed to detect NumArg mismatch"

let rec bind_vargs' params args varg env = match params, args with
  | [], [] -> env
  | p::params', a::args' -> (Env.define_var env p a |> ignore; bind_vargs' params' args' varg env)
  | [], _ ->  (Env.define_var env varg (List args) |> ignore; env)
  | _ -> raise @@ Exception.Default  "Interpreter error - failed to detect NumArg mismatch"

let bind_args (fn : Exp.fn) args =
  let pcount = List.length fn.params in
  let acount = List.length args in
  if pcount != acount && fn.varargs = None
  then raise @@ Exception.NumArgs (pcount, "" ^ string_of_int acount)
  else match fn.varargs with
    | None -> bind_args' fn.params args fn.closure
    | Some varg -> bind_vargs' fn.params args varg fn.closure


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
and apply func args = match func with
  | PrimitiveFunc fn -> fn args
  | Func fn -> List.map (f (bind_args fn args)) fn.body |> List.rev |> List.hd
  | _ -> raise @@ Exception.NotFunction ("Non-function found at front position", to_string func)
