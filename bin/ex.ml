open Lisp

let f env s =
  try Lexing.from_string s
  |> Parser.f Lexer.f
  |> Eval.f env
  |> Exp.to_string
  |> print_endline
  with
    | Exception.NumArgs (n, s) -> Printf.printf "NumArgs %d %s\n" n s
    | Exception.TypeMismatch (s1, s2) -> Printf.printf "TypeMismatch %s %s\n" s1 s2
    | Exception.LexingFail s -> Printf.printf "LexingFail %s\n" s
    | Exception.BadSpecialForm (s1, s2) -> Printf.printf "BadSpecialForm %s %s\n" s1 s2
    | Exception.NotFunction (s1, s2) -> Printf.printf "NotFunction %s %s\n" s1 s2
    | Exception.UnboundVar (s1, s2) -> Printf.printf "UnboundVar %s %s\n" s1 s2
    | Exception.Default s -> Printf.printf "DefaultError %s\n" s

let rec repl env =
  let s = (print_string "Lisp>>> "; read_line ()) in
  if s = "quit" then ()
  else (f env s; repl env)

let _ = repl (Env.create () |> Primitives.load)
