
%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token SET MAP
%token CHAN FLY REGISTER DISPATCH EXEC
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token LARROW RARROW VERTICAL LMBRACE RMBRACE FUNC
%token COLON DOT DOLLAR CLASS
%token <int> LITERAL
%token <string> ID
%token <string> STRING
%token EOF

%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%right ASSIGN
%nonassoc UMINUS
%nonassoc NOELSE
%nonassoc ELSE       /* highest precedence */

%start program
%type <Ast.program> program

%%


program:
    /*test without global stmts*/
    fdecls EOF { $1 }

fdecls:
    /*nothing*/ {([], [])}
    | fdecls fdecl {(fst $1, $2::snd $1)}

/*because we use type inferrence so we don't have vdecl*/
fdecl:
    FUNC ID LPAREN formals_opt RPAREN guards LBRACE stmt_list RBRACE
    { { typ = Undef;
    fname = $2;
    formals = $4;
    locals = [];
    guards = $6;
    body = List.rev $8 } }

guards:
    /*nothing*/ {[]}
    | VERTICAL actuals_opt {$2}

formals_opt:
 /* nothing */ { [] }
| formal_list   { List.rev $1 }

/*no type tagged default Undef, we need type inferrence*/

formal_list:
 ID                   { [(Undef, $1)] }
| formal_list COMMA ID { (Undef, $3) :: $1 }


/*need semi otherwise expr expr -> shift/reduce conflict*/
stmt_list: /*split otherwise r/r conflict */
    /*nothing*/ {[]} /*cause 60 conflict here*/
    | stmt_true_list {$1}


stmt_true_list:
    stmt SEMI {[$1]}
    | stmt SEMI stmt_true_list {$1 :: $3}

stmt:
    expr {Expr($1)}
    | RETURN expr {Return($2)}
    /*for test without else*/
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE {If ($3, $6, [])}
    | FOR LPAREN expr SEMI expr SEMI expr RPAREN LBRACE stmt_list RBRACE
       { For($3, $5, $7, $10) }
    | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
    /*for each*/
    | FOR LPAREN ID COLON expr RPAREN LBRACE stmt_list RBRACE {Foreach($3, $5, $8)}

expr_list:
    /* nothing */ {[]}
    | expr_true_list {$1}

expr_true_list:
    expr {[$1]}
    | expr_true_list COMMA expr {$3::$1}

set:
    SET LPAREN expr_list RPAREN {Set($3)}

expr_pair_list:
    /*nothing*/ {[]}
    | expr_pair_true_list {$1}

expr_pair_true_list:
    | expr COLON expr {[($1, $3)]}
    | expr_pair_true_list COMMA expr COLON expr {($3,$5)::$1}

map:
    MAP LPAREN expr_pair_list RPAREN {Map($3)}

chan_decls:
    CHAN LPAREN RPAREN {Chan()}

chan_op:
    LARROW ID {Chanunop($2)}
    | ID LARROW ID {Chanbinop($1, $3)}

fly:
    /*function_call*/
    FLY ID LPAREN actuals_opt RPAREN {Fly($2, $4)}
    /*oop_function_call*/
    | FLY ID DOT ID LPAREN actuals_opt RPAREN {Flyo($2, $4, $6)}

register:
    /*function_call*/
    REGISTER ID ID LPAREN actuals_opt RPAREN {Register($2, $3, $5)}

dispatch:
    DISPATCH ID LPAREN actuals_opt RPAREN STRING STRING {Dispatch($2, $4, $6, $7)}

exec:
    EXEC LPAREN ID RPAREN {Exec($3)}

id_list:
    ID {[$1]}
    | ID COMMA id_list {$1::$3}

lambda_expr:
    /*key word undef here*/
    LPAREN id_list RARROW expr RPAREN { Func ($2, $4)}

array:
    LMBRACE expr_list RMBRACE {Array ($2)}

expr:
    /*basic variable and const*/
    /* TODO add float */
    LITERAL { Literal($1) }
    | TRUE { BoolLit(true) }
    | FALSE { BoolLit(false) }
    | STRING { String($1) }
    | ID { Id($1)}
    | set {$1} /* set init */
    | map {$1} /* map init */
    | array {$1} /* array init */
    | lambda_expr {$1} /* lambda init */
    /*basic operation for expr*/
    | expr PLUS   expr { Binop($1, Add,   $3) }
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr LT     expr { Binop($1, Less,  $3) }
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr GT     expr { Binop($1, Greater, $3) }
    | expr GEQ    expr { Binop($1, Geq,   $3) }
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR     expr { Binop($1, Or,    $3) }
    | MINUS expr %prec UMINUS { Unop(Neg, $2) }
    | NOT expr         { Unop(Not, $2) }
    /*assign expr*/
    | ID ASSIGN expr   { Assign($1, $3) }
    /*function call*/
    | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
    /*oop function call*/
    | ID DOT ID LPAREN actuals_opt RPAREN {ObjCall($1, $3, $5)}
    /*expression is contained with () */
    | LPAREN expr RPAREN { $2 }
    /*network syntax*/
    | chan_decls {$1}
    | chan_op {$1}
    | fly {$1}
    | register {$1}
    | dispatch {$1}
    | exec {$1}

actuals_opt:
    /*nothing*/ {[]}
    | actuals_list {List.rev $1}

actuals_list:
    expr                    { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }

/* TODO list comprehension add to expr*/


/* TODO pattern match add to expr*/

/* TODO chan() expr */
/*  ID <- expr */
