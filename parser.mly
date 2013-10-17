%{ open Ast %}

/*
 * Some tokens carry a value with them
 * <int> LITERAL is carrying a value
 */
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

/*
 * The start symbol is represented here
 * The type is the member program of Ast Module
 * OCaml reminder: each source file is a module
 * and everything is public
 */
%start program
%type <Ast.program> program

%%

/*
 * program will recursively match for variable declarations
 * or function declarations. They will be chained together
 * in a OCaml list. At first appending to an empty list
 * And then appending to the list with the last item first
 * appending the list in backwards chronological order
 */
program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

/*
 * fdecl returns a type func_decl defined in ast.ml
 * a kind of c struct
 *
 * The locals and the body will come back in reverse order
 * So we flip them around to happen in the correct chronological
 * order written in the source code
 * 
 */
fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

/*
 * If there are several global variables a list will be created
 * by the program -> program vdecl production
 *
 */
vdecl:
   INT ID SEMI { $2 }

/*
 * When stmt_list reaches the end of the line (its run out of statements)
 * then it returns an empty list. The empty list is then the beginning of
 * the statement list that will be appended to
 * so the frist appending looks something like this
 * 
 * stmt :: []
 * 
 * Now the statement list has the first line of the program essentially
 * at the back of the list (which will later be reversed above. 
 * recursively this stmt_list is built up by appending stmt after stmt
 * 
 * so we end with something like with real values
 *
 * { [Call("print", [Expr(Id("a"))]) ; Expr(Assign("a", Literal(42)))] }
 *
 */
stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
