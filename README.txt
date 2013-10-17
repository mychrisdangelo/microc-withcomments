Quickstart

1. $ echo "int a; main() { print(a); }" > helloworld.mc
2. $ make
3. $ ./microc < helloworld.mc

Running the Debugger

1. $ ocamldebug ./microc
2. (ocd) set arguments < helloworld.mc
3. (ocd) break @ microc 52
4. (ocd) run

Example Session in the Debugger for a peak at the parsed program:

> ocamldebug ./microc
        OCaml Debugger version 4.01.0

(ocd) set arguments < helloworld.mc
(ocd) break @ microc 52
Loading program... done.
Breakpoint 1 at 59788: file microc.ml, line 45, characters 16-41
(ocd) run
Time: 25 - pc: 59792 - module Microc
Breakpoint: 1
52   <|b|>let program = Parser.program Scanner.token lexbuf in
(ocd) print program
Unbound identifier program
(ocd) n
Time: 17138 - pc: 59824 - module Microc
53   <|b|>match action with
(ocd) print program
program: Ast.program =
  (["a"],
   [{Ast.fname = "main"; formals = []; locals = ["b"];
     body =
      [Ast.Expr (Ast.Assign ("b", Ast.Literal 36));
       Ast.Expr (Ast.Assign ("a", Ast.Literal 42));
       Ast.Expr (Ast.Call ("print", [Ast.Id "a"]))]}])
