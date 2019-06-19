module Envm = Map.Make(String)

type expt = Atom of string
          | List of expt list
          | DottedList of expt list * expt
          | Number of int
          | String of string
          | Bool of bool
          | PrimitiveFunc of (expt list -> expt)
          | Func of fn
          | PortIn of in_channel
          | PortOut of out_channel
and envt = (expt ref) Envm.t ref
and fn = {params : string list;
          varargs : string option;
          body : expt list;
          closure : envt}
