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
