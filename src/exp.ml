type t = Atom of string
       | List of t list
       | DottedList of t list * t
       | Number of int
       | String of string
       | Bool of bool
