{
open Parser
}

let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let int = '-'? digit | ['1'-'9'] digit+
let symbol = ['!' '#' '$' '%' '&' '|' '*' '+' '-' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~']
let char = ['a'-'z' 'A'-'Z']
let atom = (symbol | char) (symbol | char | digit)*
let badatom = digit+ (symbol | char) (symbol | char | digit)*
let string = '"' [^ '"']* '"'

rule f = parse
  | "'" { QUOTE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { DOT }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | badatom { raise @@ Exception.LexingFail "Atoms cannot start with a digit" }
  | int as n { INT (int_of_string n) }
  | atom as a { ATOM a }
  | string as s { STRING s }
  | whitespace* { f lexbuf }
  | eof { EOF }
  | _ { raise @@ Exception.LexingFail "Invalid char encountered while lexing" }
