
%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR SADD
%token SET MAP AT
%token MOD
%token LJINHAO RJINHAO
%token NULL SCOPE
%token CHAN FLY REGISTER DISPATCH
%token RETURN IF ELSE FOR WHILE
%token BREAK CONTINUE
%token LARROW RARROW VERTICAL LMBRACE RMBRACE FUNC
%token COLON DOT DOLLAR CLASS
%token <int> LITERAL
%token <string> ID
%token <string> STRING
%token <float> FLOAT
%token EOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ SADD
%left MOD
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%nonassoc UMINUS
%nonassoc NOELSE
%nonassoc ELSE       /* highest precedence */

%start program
%type <Ast.program> program

%%


program:
    /*test without global stmts*/
    stmt_list cdecls fdecls EOF {Program($2, $3)}

fdecls:
    /*nothing*/ {[]}
    | fdecls fdecl {$2 :: $1}

/*because we use type inferrence so we don't have vdecl*/
fdecl:
    FUNC ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { {
    fname = $2;
    formals = $4;
    body = $7 } }




variable_ref:
    ID COLON typedef {
        ($1, $3)
    }


variable_defs_opt:
    /*nothing*/ {[]}
    | variable_defs {$1}

variable_defs:
    variable_ref SEMI{[$1]}
    | variable_ref SEMI variable_defs {$1 :: $3}

typedef_list:
    typedef {[$1]}
    | typedef_list COMMA typedef {$3::$1}

typedef_list_opt:
    /* nothing*/ {[]}
    | typedef_list {List.rev $1}

/*typedef description*/
typedef:
    ID {
        match $1 with
        | "Int" -> Int
        | "Bool" -> Bool
        | "Void" -> Void
        | "String" -> String
        | "Float" -> Float
        | "Map" | "Set" -> failwith ("set map init must with parameters")
        | x -> Class x
    }
    | ID LJINHAO typedef_list_opt RJINHAO {
        match $1 with
        | "Set" -> begin
                match $3 with
                |[x] -> Set x
                | _ -> failwith ("set just with one parameter")
                end
        | "Map" -> begin
                match $3 with
                | [x;y] -> Map (x,y)
                | _ -> failwith ("map just two parameter")
                end
        | "Array" -> begin
               match $3 with
               |[x] -> Array x
               | _ -> failwith ("array just with one parameter")
               end
        | _ -> failwith ("not suppport template except set map")
    }
cdecls:
    /* nothing*/ {[]}
    | cdecls cdecl {$2 :: $1}

/* class definition */
cdecl:
    CLASS ID LBRACE variable_defs_opt fdecls RBRACE {
        {
            cname = $2;
            func_decls = $5;
            member_binds = $4
        }
    }

formals_opt:
 /* nothing */ { [] }
| formal_list   { List.rev $1 }

/*no type tagged default Undef, we need type inferrence*/

formal_list:
 ID                   { [$1] }
| formal_list COMMA ID { $3 :: $1 }


/*need semi otherwise expr expr -> shift/reduce conflict*/
stmt_list: /*split otherwise r/r conflict */
    /*nothing*/ {[]} /*cause 60 conflict here*/
    | stmt_true_list {$1}

ctrlblock:
    FOR LPAREN expr SEMI expr SEMI expr RPAREN LBRACE stmt_list RBRACE
       { For($3, $5, $7, $10) }
    | WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
    | LBRACE stmt_list RBRACE {Block($2)}
    /*for test without else*/
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE {If ($3, $6, [])}
    | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE {If ($3, $6, $10)}
    /*for each*/
    | FOR LPAREN ID COLON expr RPAREN LBRACE stmt_list RBRACE {Foreach($3, $5, $8)}

stmt_true_list:
    stmt SEMI {[$1]}
    | stmt SEMI stmt_true_list {$1 :: $3}
    | ctrlblock {[$1]}
    | ctrlblock stmt_true_list {$1 :: $2}

stmt:
    expr {Expr($1)}
    | BREAK {Break}
    | CONTINUE {Continue}
    | RETURN expr {Return($2)}

expr_list:
    /* nothing */ {[]}
    | expr_true_list {$1}

expr_true_list:
    expr {[$1]}
    | expr_true_list COMMA expr {$3::$1}

set:
    SET LPAREN expr_list RPAREN {Set(List.rev $3)}

expr_pair_list:
    /*nothing*/ {[]}
    | expr_pair_true_list {$1}

expr_pair_true_list:
    | expr COLON expr {[($1, $3)]}
    | expr_pair_true_list COMMA expr COLON expr {($3,$5)::$1}

map:
    MAP LPAREN expr_pair_list RPAREN {Map(List.rev $3)}



fly:
    /*function_call*/
    FLY ID LPAREN actuals_opt RPAREN {Fly($2, $4)}
    /*oop_function_call*/
    | FLY ID DOT ID LPAREN actuals_opt RPAREN {Flyo($2, $4, $6)}

register:
    /*function_call*/
    REGISTER ID ID LPAREN actuals_opt RPAREN {Register($2, $3, $5)}

dispatch:
    DISPATCH ID LPAREN actuals_opt RPAREN {
        let arr = List.rev $4
        in match arr with
        | x::y::z -> Dispatch($2, List.rev z, y, x)
        | _ -> failwith ("dispatch param error")
        }

id_list:
    ID {[$1]}
    | ID COMMA id_list {$1::$3}

lambda_expr:
    /*key word undef here*/
    LPAREN id_list RARROW expr RPAREN { Func ($2, $4)}

array:
    LMBRACE expr_list RMBRACE {Array (List.rev $2)}


list_comprehen:
    LMBRACE expr VERTICAL ID LARROW expr RMBRACE { ListComprehen($2, $4, $6)}

assign_expr:
    /*assign expr*/
    ID ASSIGN expr   { Assign($1, $3) }
mvar_assign_expr:
    /*member assign expr*/
    ID DOT ID ASSIGN expr {MAssign ($1, $3, $5)}

expr:
    /*basic variable and const*/
    /* TODO add float */
    LITERAL { Literal($1) }
    | TRUE { BoolLit(true) }
    | ID SCOPE NULL { Null($1)}
    | FALSE { BoolLit(false) }
    | STRING { String($1) }
    | FLOAT {Float($1)}
    | ID { Id($1)}
    | set {$1} /* set init */
    | map {$1} /* map init */
    | array {$1} /* array init */
    | lambda_expr {$1} /* lambda init */
    | list_comprehen {$1} /* list comprehension */
    | assign_expr {$1} /* assign expr */
    | mvar_assign_expr {$1} /* member variale assign expr*/
    /*basic operation for expr*/
    | expr PLUS   expr { Binop($1, Add,   $3) }
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr LT     expr { Binop($1, Less,  $3) }
    | expr SADD expr {Binop($1, SAdd, $3)}
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr GT     expr { Binop($1, Greater, $3) }
    | expr GEQ    expr { Binop($1, Geq,   $3) }
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR     expr { Binop($1, Or,    $3) }
    | expr MOD expr {Binop($1, Mod, $3)}
    | MINUS expr %prec UMINUS { Unop(Neg, $2) }
    | NOT expr         { Unop(Not, $2) }
    /*function call*/
    | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
    /*class member function call*/
    | ID DOT ID LPAREN actuals_opt RPAREN {ObjCall($1, $3, $5)}
    /*class member get*/
    | ID DOT ID {Objid($1, $3)}
    /*class generation syntax*/
    | AT typedef {ObjGen($2)}
    /*class generation syntax*/
    /*expression is contained with () */
    | LPAREN expr RPAREN { $2 }
    /*network syntax*/
    | chan_decls {$1}
    | chan_op {$1}
    | fly {$1}
    | register {$1}
    | dispatch {$1}

chan_decls:
    CHAN LPAREN typedef RPAREN {Changen($3)}

chan_op:
    LARROW ID {Chanunop($2)}
    | ID LARROW ID {Chanbinop($1, $3)}

actuals_opt:
    /*nothing*/ {[]}
    | actuals_list {List.rev $1}

actuals_list:
    expr                    { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }
