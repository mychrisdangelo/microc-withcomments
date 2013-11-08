Quickstart

1. $ echo "int a; main() { print(a); }" > helloworld.mc
2. $ make
3. $ ./microc < helloworld.mc

Running the Debugger

1. $ ocamldebug ./microc
2. (ocd) set arguments < hello.mc
3. (ocd) break @ microc 52
4. (ocd) run

Example Session in the Debugger for a peak at the parsed program

> ocamldebug ./microc
        OCaml Debugger version 4.01.0

(ocd) set arguments < hello.mc
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

Example Session Generating Bytecode:

> ./microc -b < hello2.mc
1 global variables
0 Jsr 2
1 Hlt
2 Ent 1
3 Lit 42
4 Sfp 1
5 Drp
6 Lfp 1
7 Jsr 15
8 Str 0
9 Drp
10 Lod 0
11 Jsr -1
12 Drp
13 Lit 0
14 Rts 0
15 Ent 1
16 Lit 1
17 Sfp 1
18 Drp
19 Lfp -2
20 Lfp 1
21 Add
22 Rts 1
23 Lit 0
24 Rts 1

Credits:

Stephen Edwards Micro C Compiler: http://www.cs.columbia.edu/~sedwards/classes/2013/w4115-fall/microc.tar.gz
Stephen Edwards Slides on Micro C Compiler: http://www.cs.columbia.edu/~sedwards/classes/2013/w4115-fall/microc.pdf
Stephen Edwards Slides on OCaml: http://www.cs.columbia.edu/~sedwards/classes/2013/w4115-fall/ocaml.pdf
Jason Hickey's, Introduction to OCaml: http://www.cs.caltech.edu/courses/cs134/cs134b/book.pdf
