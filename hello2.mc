/*
 * program: Ast.program =
 * (["a"],
 *  [{Ast.fname = "main"; formals = []; locals = ["b"];
 *    body =
 *     [Ast.Expr (Ast.Assign ("b", Ast.Literal 42));
 *      Ast.Expr (Ast.Assign ("a", Ast.Call ("inc", [Ast.Id "b"])));
 *      Ast.Expr (Ast.Call ("print", [Ast.Id "a"]))]};
 *   {Ast.fname = "inc"; formals = ["x"]; locals = ["y"];
 *    body =
 *     [Ast.Expr (Ast.Assign ("y", Ast.Literal 1));
 *      Ast.Return (Ast.Binop (Ast.Id "x", Ast.Add, Ast.Id "y"))]}])
 */

/*
 *   Byte Code pre "fun_offset_list":
 *
 *   (ocd) print func_bodies
 *		   func_bodies: Bytecode.bstmt list list =
 *         [[Jsr 1; Hlt];
 *          [Ent 1; Lit 42; Sfp 1; Drp; Lfp 1; Jsr 2; Str 0; Drp; Lod 0; Jsr (-1); Drp; Lit 0; Rts 0];
 *          [Ent 1; Lit 1; Sfp 1; Drp; Lfp (-2); Lfp 1; Bin Add; Rts 1; Lit 0; Rts 1]]
 * 
 *   Byte Code Printed using bytecode.ml "string_of_prog" with comments:
 *
 *	 1 global variables   # first just print the number of globals
 *   0 Jsr 2 			  # Call function absolute address 2
 *   1 Hlt				  
 *   2 Ent 1    		  # Main begins. Allocate Stack space for 1 local.
 *   3 Lit 42			  # Literal 42
 *   4 Sfp 1              # Store in location 1 relative to FP
 *   5 Drp				  # Discard value
 *   6 Lfp 1              # Load Value from position FP+1
 *   7 Jsr 15             # Call function from absolute address 15
 *   8 Str 0
 *   9 Drp
 *   10 Lod 0             # Fetch Global Variable 0
 *   11 Jsr -1            # print
 *   12 Drp				  #
 *   13 Lit 0			  # default = return 0
 *   14 Rts 0			  # Restore File Pointer
 *   15 Ent 1
 *   16 Lit 1
 *   17 Sfp 1
 *   18 Drp
 *   19 Lfp -2
 *   20 Lfp 1
 *   21 Add
 *   22 Rts 1
 *   23 Lit 0
 *   24 Rts 1
 */

int a;

inc(x)
{
  int y;
  y = 1;
  return x + y;
}

main()
{
  int b;
  b = 42;
  a = inc(b);
  print(a);
}
