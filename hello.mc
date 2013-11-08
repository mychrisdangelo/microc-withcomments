/*
 * Tokens created by the Scanner for this program:
 *
 * INT
 * ID("a")
 * SEMI
 * ID("main")
 * LPAREN
 * RPAREN
 * LBRACE
 * ID("a")
 * ASSIGN
 * LITERAL(42)
 * SEMI
 * ID("print")
 * LPAREN
 * ID("a")
 * RPAREN
 * SEMI
 * RBRACE
 *
 * Semantic Actions performed by the parser for this program
 * Includes path of derivation (_2) just references the path of the derivation:
 *
 * program -> program_2 fdecl
 * { ["a"], [ { fname = "main"; 
 *     			formals = []; 
 *     			locals = []; 
 *     			body = [ Expr(Assign("a", Literal(42))) ; 
 *              Call("print", [ Expr(Id("a)) ]) ] } ] }
 *
 * program_2 -> program_3 vdecl
 * { ["a"], [] }
 *
 * program_3 -> nothing
 * { [], [] }
 *
 * vdecl -> INT ID SEMI
 * { "a" }
 *
 * fdecl -> ID("main") LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
 * { { fname = "main"; 
 *     formals = []; 
 *     locals = []; 
 *     body = [ Expr(Assign("a", Literal(42))) ; 
 *              Call("print", [ Expr(Id("a)))])
 *              [] ]; } }
 *
 * formals_opt -> nothing
 * { [] }
 *
 * vdecl_list -> nothing
 * { [] }
 *
 * stmt_list -> stmt_list_1 stmt_1
 * { [ Call("print", [ Expr(Id("a")) ]) ; Expr(Assign("a", Literal(42))) ] }
 *
 * stmt_list_1 -> stmt_list_2 stmt_2
 * { [ Call("print", [ Expr(Id("a")) ]) ] }
 *
 * stmt_1 -> expr_1 SEMI
 * { Expr(Assign("a", Literal(42))) }
 *
 * expr_1 -> ID ASSIGN expr_2
 * { Assign("a", Literal(42)) } 
 *
 * expr_2 -> LITERAL
 * { Literal(42) }
 *
 * stmt_2 -> expr_3 SEMI
 * { Expr(Call("print", [ Expr(Id("a")) ])) } 
 *
 * expr_3 -> ID LPAREN actuals_opt RPAREN
 * { Call("print", [ Expr(Id("a")) ]) }
 *
 * actuals_opt -> actuals_list
 * { [ Expr(Id("a")) ] }
 *
 * actuals_list -> expr_4
 * { [ Expr(Id("a")) ] }
 *
 * expr_4 -> ID
 * { Id("a") }
 *
 * 
 *
 */

int a;
main()
{
   a = 42;
   print(a);
}
