%token <int> INT
%token <string> ATOM
%token <string> STRING
%token LPAREN RPAREN QPAREN
%token DOT
%token TRUE FALSE
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
  | a = ATOM { Exp.Atom a }
  | n = INT { Exp.Number n }
  | s = STRING { Exp.String s }
  | TRUE { Exp.Bool true }
  | FALSE { Exp.Bool false }
  | LPAREN; es = list(expr); RPAREN { Exp.List es }
  | LPAREN; es = list(expr); DOT; e = expr; RPAREN { Exp.DottedList (es, e) }
  | QPAREN; es = list(expr); RPAREN { Exp.List (Exp.Atom "quote" :: es) }
