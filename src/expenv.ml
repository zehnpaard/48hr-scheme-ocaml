module Envm = Map.Make(String)

type expt = Atom of string
          | List of expt list
          | DottedList of expt list * expt
          | Number of int
          | String of string
          | Bool of bool
and envt = expt Envm.t ref
