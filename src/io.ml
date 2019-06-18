open Exp

let make_port_in = function
  | [String s] -> PortIn (open_in s)
  | _ -> raise @@ Exception.TypeMismatch ("string", "")

let make_port_out = function
  | [String s] -> PortOut (open_out s)
  | _ -> raise @@ Exception.TypeMismatch ("string", "")

let close_port_in = function
  | [PortIn c] -> (close_in c; Bool true)
  | _ -> Bool false
  
let close_port_out = function
  | [PortOut c] -> (close_out c; Bool true)
  | _ -> Bool false

let read_exp s = Lexing.from_string s |> Parser.f Lexer.f
let read_exps s = Printf.sprintf "(%s)" s |> read_exp

let rec read_proc = function
  | [] -> read_proc [PortIn stdin]
  | [PortIn c] -> (try input_line c with End_of_file -> "'()")
                  |> read_exp
  | _ -> raise @@ Exception.TypeMismatch ("Input Port", "")

let rec write_proc = function
  | [x] -> write_proc [x; PortOut stdout]
  | [x; PortOut c] -> (to_string x
                       |> Printf.fprintf c "%s\n"
                       |> ignore;
                       Bool true)
  | _ -> raise @@ Exception.TypeMismatch ("Input Output", "")

let read fn =
  let inf = open_in fn in
  let rec read' acc = match input_line inf with
    | s -> read' (s :: acc)
    | exception End_of_file -> List.rev acc |> String.concat "\n"
  in
  read' []

let read_contents = function
  | [String filename] -> String (read filename)
  | _ -> raise @@ Exception.TypeMismatch ("string", "")

let read_all = function
  | [String filename] -> read filename |> read_exps
  | _ -> raise @@ Exception.TypeMismatch ("string", "")

let load s = match read_all [String s] with
  | List xs -> xs
  | _ -> raise @@ Exception.Default "read-all returned non-list"
