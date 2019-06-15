open Exp

let f e = match e with
  | String _ -> e
  | Number _ -> e
  | Bool _ -> e
  | List [Atom "quote"; e2] -> e2
  | _ -> failwith "Evaluation not implemented"
