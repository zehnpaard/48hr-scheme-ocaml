open Lisp

let _ =
  Lexing.from_channel stdin
  |> Parser.f Lexer.f
  |> Exp.to_string
  |> print_endline
