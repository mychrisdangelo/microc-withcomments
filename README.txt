Quickstart

1. $ echo "int a; main() { print(a); }" > helloworld.mc
2. $ make
3. $ ./microc < helloworld.mc

Running the Debugger

1. $ ocamldebug ./microc
2. (ocd) set arguments < helloworld.mc
3. (ocd) break @ microc 51
4. (ocd) run

