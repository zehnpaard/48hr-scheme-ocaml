type t = (string, Exp.t) Hashtbl.t

let create () = (Hashtbl.create 100 : t)

let is_bound = Hashtbl.mem
let get_var env v = match Hashtbl.find_opt env v with
  | Some e -> e
  | None -> raise @@ Exception.UnboundVar ("Getting unbound var", v)

let set_var env v e = 
  if is_bound env v
  then (Hashtbl.replace env v e; e)
  else raise @@ Exception.UnboundVar ("Setting unbound var", v)

let define_var env v e = (Hashtbl.replace env v e; e)

let bind_vars env ves =
  let f (v, e) = Hashtbl.replace env v e in
  List.iter f ves
