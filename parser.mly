
%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token ARROW VERTICAL LMBRACE RMBRACE FUNC
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
stmt_list:
    /* nothing */  { [] }
    | stmt {[$1]}
    | stmt_list SEMI stmt { $3 :: $1 }

stmt:
    expr {Expr($1)}
    | RETURN expr {Return($2)}
    /*for test without else*/
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE {If ($3, $6, [])}
    | FOR LPAREN expr SEMI expr SEMI expr RPAREN LBRACE stmt_list RBRACE
       { For($3, $5, $7, $10) }
    | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
    /*for each*/
    | FOR LPAREN ID COLON expr LBRACE stmt_list RBRACE {Foreach($3, $5, $7)}

expr:
    /*basic variable and const*/
    LITERAL { Literal($1) }
    | TRUE { BoolLit(true) }
    | FALSE { BoolLit(false) }
    | STRING { String($1) }
    | ID { Id($1)}
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


actuals_opt:
    /*nothing*/ {[]}
    | actuals_list {List.rev $1}

actuals_list:
    expr                    { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }
