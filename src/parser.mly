%token <int> INT
%token <string> ATOM
%token <string> STRING
%token LPAREN RPAREN
%token DOT
%token QUOTE
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
  | a = ATOM { if a = "#t" then Exp.Bool true
               else if a = "#f" then Exp.Bool false
               else Exp.Atom a }
  | n = INT { Exp.Number n }
  | s = STRING { Exp.String (String.sub s 1 (String.length s - 2)) }
  | LPAREN; es = list(expr); RPAREN { Exp.List es }
  | LPAREN; es = list(expr); DOT; e = expr; RPAREN { Exp.DottedList (es, e) }
  | QUOTE; e = expr { Exp.List [Exp.Atom "quote"; e] }
