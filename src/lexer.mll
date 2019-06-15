{
open Parser
}

let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let int = digit | ['1'-'9'] digit+
let symbol = ['!' '#' '$' '%' '&' '|' '*' '+' '-' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~']
let char = ['a'-'z' 'A'-'Z']
let atom = (symbol | char) (symbol | char | digit)*
let badatom = digit+ (symbol | char) (symbol | char | digit)*
let string = '"' [^ '"']* '"'

rule f = parse
  | "'(" { QPAREN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { DOT }
  | "true" { TRUE }
  | "false" { FALSE }
  | badatom { failwith "Atoms cannot start with a digit" }
  | atom as a { ATOM a }
  | int as n { INT (int_of_string n) }
  | string as s { STRING s }
  | whitespace* { f lexbuf }
  | eof { EOF }
  | _ { failwith "Invalid char encountered while lexing" }
