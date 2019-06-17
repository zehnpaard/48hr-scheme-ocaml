module M = Expenv.Envm
type t = Expenv.envt

let create () = (ref @@ M.empty : t)
let copy env = ref !env

let is_bound env v = M.mem v !env
let get_var env v = match M.find_opt v !env with
  | Some e -> e
  | None -> raise @@ Exception.UnboundVar ("Getting unbound var", v)

let set_var env v e = 
  if is_bound env v
  then (env := M.add v e !env; e)
  else raise @@ Exception.UnboundVar ("Setting unbound var", v)

let define_var env v e = (env := M.add v e !env; e)

let bind_vars env ves =
  let f (v, e) = env := M.add v e !env in
  List.iter f ves
