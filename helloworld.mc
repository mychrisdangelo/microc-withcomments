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
 * fdecl -> ID("main") LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
 *
 *
 * formals_opt -> nothing
 * { [] }
 *
 * vdecl_list -> nothing
 * { [] }
 *
 * stmt_list -> expr_1
 *
 * expr_1 -> ID ASSIGN expr_2
 * { Assign("a", 
 *
 * expr_2 -> LITERAL
 * { Literal(42) }
 *
 *
 *
 * program_2 -> program_1 vdecl
 * vdecl -> INT ID("a") SEMI 
 * { "a" }
 *  
 * program_1 -> nothing
 * { [], [] } 
 *
 *
 */




int a;
main()
{
   a = 42;
   print(a);
}
