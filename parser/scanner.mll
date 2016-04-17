{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LMBRACE}
| ']'      { RMBRACE} (*add [] for array init*)
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| ':'      { COLON }
| '.'      { DOT } (*for oop call*)
| '|'      { VERTICAL} (* for guard *)
| '$'      { DOLLAR } (* for set initialization *)
| "set"    { SET } (* for set definition*)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "@"      { AT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "%"      { MOD }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "<#"     { LJINHAO}
| "#>"     { RJINHAO}
| "->"     { RARROW } (* for lambda expression *)
| "<-"     { LARROW } (* for chan *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "::" {SCOPE}
| "while"  { WHILE }
| "return" { RETURN }
| "true"   { TRUE }
(*add null support*)
| "null" {NULL}
| '^' {SADD}
| "false"  { FALSE }
| "class"  { CLASS } (*for class initialization *)
| "func" {FUNC} (*declaration for function*)
| "map" {MAP} (*declaration for map*)
| "chan" {CHAN}
| "fly" {FLY}
| "register" {REGISTER}
| "dispatch" {DISPATCH}
| "exec" {EXEC}
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
(* float scan TODO *)
| ['0'-'9']+ '.' ['0'-'9']+ as lxm {FLOAT(float_of_string lxm)}
| ['\"'] [^'\"']* ['\"'] as lxm {STRING(lxm)}
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
