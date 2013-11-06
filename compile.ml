open Ast
open Bytecode

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
(*
 * OCaml reminder: this type is a "record" which is like a
 * C Struct.
 *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

(* 
 * val enum : int -> 'a list -> (int * 'a) list 
 * (output is a (int, some_type) list)
 * OCaml Reminder Example:
 * The type identity : 'a -> 'a says that the identity function
 * takes an argument of some arbitrary type 'a and returns a value
 * of the same type 'a
 * 
 * example input: (enum 1 0 ["a", "b"])
 * hd = "a"
 * tl = ["b"];
 * stride = 1
 * n = 0
 *
 * (0, "a") :: enum  1 (0+1) "a"
 * ...
 * finally returning [(0, "a"); (1, "b")]
 *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* 
 * val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a
 *
 * example input: StringMap.empty [(0, "a"); (1, "b")]
 * 
 * round 1:
 *  m = StringMap.empty
 *  i = 0 (from pairs = (0, "a"))
 *  n = "a" (from pairs = (0, "a"))
 *  StringMap.add "a" 0 StringMap.emtpy
 * round 2:
 *  m = (this is my own map syntax) [("a" : 0)]
 *  i = 1 
 *  n = "b"
 * ...
 *  finally returning [("a": 0); ("b": 1)]
 *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
(*
 * A program in AST form (that will be fed into this program) will
 * look something like this
 * 
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
 *
 *)
let translate (globals, functions) =

  (* 
   * Allocate "addresses" for each global variable 
   * 
   * global_index will be something like this (this is my own
   * string map syntax) [("a": 0); ("b": 1)]
   *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in

  (* 
   * Assign indexes to function names; built-in "print" is special 
   * 
   * first start with "print" as a built in function with position -1
   * this is fed as the starting map to build the rest of the function
   * map. 
   *
   * Ocaml reminder: List.map f [a1; ... ; an] = [f a1; ... ;f an]
   * So in the context below the function list is a bunch of Ast.func_decls
   * iteratively stored in f we return the f.name
   * example: ["main"; "inc"]
   * 
   * enum goes through and assigns a number returnign something like this:
   * [(1, "main"); (2, "inc")]
   * 
   * later this is put into string map pairs like global indexes finally
   * resulting in a map that looks like this (my madeup syntax):
   * [("print": -1); ("main": 1); ("inc": 2)]
   * 
   * to inspect the above you could insert this expression
   * ignore (print_int (StringMap.find "main" function_indexes));
   *
   *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in

  (* 
   * Translate a function in AST form into a list of bytecode statements 
   * 
   * See FIG.1 below for where this function is called
   *
   * env is of "env" record type defined above.
   *
   *)
  let translate env fdecl =
    (* 
     * Bookkeeping: FP offsets for locals and arguments 
     *)
    (* 
     * OCaml reminder:
     * in the below the use of keywords "let" and "and" are used
     * to chain a bunch of local variables.
     * Example where the application is close by:
     * # let x = 3 and y = 4 in x + y;;
     * - : int = 7
     * 
     * Reminder that (enum 1 1 fdecl.locals) will retrun a list of
     * tuples with the "offset" for each local variable
     * in our running example for "main": [(1, "b")]
     *
     * the formal_offsets use a different 'protocol' starting the offset
     * from -1 and incrementing each point by -2. So for the "inc" function
     * in our running example: [(-1, "x")]
     *)
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and local_offsets = enum 1 1 fdecl.locals
    and formal_offsets = enum (-1) (-2) fdecl.formals in 
    (* in: num_formals, num_locals, local_offsets, formal_offsets *)
    (*
     * OCaml reminder:
     * a record can be filled manually using the keyword with
     * below env is being set to an env type with the "c-struct-like-item"
     * local_index set to the result of string_map_pairs
     *
     * OCaml reminder
     * the @ symbol is used to concatentate to lists. example:
     * [1; 2] @ [3; 4];; gives [1; 2; 3; 4]
     *
     * So in our example of inc we have just concatenated a list
     * of our formals with locals (because at this point) we 
     * think of them as the same scope of locals. So we have now
     * formed a list like this for function "inc" [("y", 1); ("x", -1)]
     * which is set to local_index
     *)
    let env = { env with local_index = string_map_pairs
		  StringMap.empty (local_offsets @ formal_offsets) } in (* env *)

    (*
     * Finally the heart of the the translate function.
     *
*)finishHere
     * 
     *)
    let rec expr = function
	Literal i -> [Lit i]
      | Id s ->
	  (try [Lfp (StringMap.find s env.local_index)]
          with Not_found -> try [Lod (StringMap.find s env.global_index)]
          with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> expr e1 @ expr e2 @ [Bin op]
      | Assign (s, e) -> expr e @
	  (try [Sfp (StringMap.find s env.local_index)]
  	  with Not_found -> try [Str (StringMap.find s env.global_index)]
	  with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Call (fname, actuals) -> (try
	  (List.concat (List.map expr (List.rev actuals))) @
	  [Jsr (StringMap.find fname env.function_index) ]   
        with Not_found -> raise (Failure ("undefined function " ^ fname)))
      | Noexpr -> []

    in (* expr *)
    (*
     * Using our running example what's passed in from main is
     * (Block [Ast.Expr (Ast.Assign ("b", Ast.Literal 42));
     *                   Ast.Expr (Ast.Assign ("a", Ast.Call ("inc", [Ast.Id "b"])));
     *                   Ast.Expr (Ast.Call ("print", [Ast.Id "a"])])
     *  
     * First round:
     * We pattern match Block sl
     * sl = [Ast.Expr (Ast.Assign ("b", Ast.Literal 42));
     *                 Ast.Expr (Ast.Assign ("a", Ast.Call ("inc", [Ast.Id "b"])));
     *                 Ast.Expr (Ast.Call ("print", [Ast.Id "a"])]
     *
     * The below is the result of the pattern match
     * List.concat (List.map stmt sl) 
     * the map function will be used to run the function stmt over each
     * item in the sl list. And will List.concat will concatenate the list together
     * piece by piece.
     *
     * Second Round (first item in sl list):
     * We pattern match to Ast.Expr. the result of this is a call to the function
     * defined above expr (Ast.Expr (Ast.Assign ("b", Ast.Literal 42)) @ [Rts 0]
     * we continue this example above in the expr function
     * 
     *)
    let rec stmt = function
	Block sl     ->  List.concat (List.map stmt sl)
      | Expr e       -> expr e @ [Drp]
      | Return e     -> expr e @ [Rts num_formals]
      | If (p, t, f) -> let t' = stmt t and f' = stmt f in
	expr p @ [Beq(2 + List.length t')] @
	t' @ [Bra(1 + List.length f')] @ f'
      | For (e1, e2, e3, b) ->
	  stmt (Block([Expr(e1); While(e2, Block([b; Expr(e3)]))]))
      | While (e, b) ->
	  let b' = stmt b and e' = expr e in
	  [Bra (1+ List.length b')] @ b' @ e' @
	  [Bne (-(List.length b' + List.length e'))]

    (*
     * !! CALLER OF stmt FUNCTION (which consequently calls expr)
     * !! THIS IS THE RETURN VALUE of "let translate env fdecl ="
     * 
     * OCaml reminder: @ concatenates two lists
     *
     * Ent, Block, Rts are all defined in the bytecode.ml
     * example: (Ent value) is using the value in the constructor Ent
     * The return value of all these concatenated items is bstmt lists
     * so the concatenate operation is satisfied b/c they are all the same
     * ocaml type
     * 
     * The result for main in our running example is:
     * [Ent 1] @ 
     * [stmt (Block [Ast.Expr (Ast.Assign ("b", Ast.Literal 42));
     *               Ast.Expr (Ast.Assign ("a", Ast.Call ("inc", [Ast.Id "b"])));
     *               Ast.Expr (Ast.Call ("print", [Ast.Id "a"])])] @
     * [Lit 0; Rts 0]:q
     *)
    in [Ent num_locals] @      (* Entry: allocate space for locals *)
    stmt (Block fdecl.body) @  (* Body *)
    [Lit 0; Rts num_formals]   (* Default = return 0 *)

  in (* let translate env fdecl = *)
  let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in (* env_2 overwrites env *)

  (* 
   * Code executed to start the program: Jsr main; halt 
   * 
   * Ocaml reminder:
   * Jsr is a special algebraic data type that has been declared in
   * the file bytecode.ml simplified here
   *
   * type bstmt =
   *     ...
   *    | Jsr of int    (* Call function by absolute address *)
   *
   * So, Jsr is essentially an int. Note that tage names must be globally unique
   * requirement for type inference. And they must begin with a capital letter.
   * This Identifier is also called the constructor name. THat's right Jsr is
   * a constructor. So Jsr (3) will create a Jsr type of value 3.
   *
   * Another OCaml simple reminder:
   * try ... with is a try ... catch block. Not_found is the type of Exception
   * we're expected to catch. Failure is a an exception type. that we are
   * throwing ourselves instead of letting the standard "Not_found" bubble up
   *
   * The try block works like this:
   * We get the function index of "main" (which in our example is 1).
   * Jsr is a constructor that forms a Jsr type of value 1. The result of this
   * try block is [Jsr(1); Hlt]
   * 
   * Hlt is another type defined in bytecode.ml that is used to communicate
   * end of execution is here "Halt".
   *)
  let entry_function = try
    [Jsr (StringMap.find "main" function_indexes); Hlt]
  with Not_found -> raise (Failure ("no \"main\" function"))
  in (* entry_function *)
    
  (* 
   * Compile the functions
   * 
   * FIG.1
   * (translate env) is actually the call of the function defined above
   *  let translate env fdecl =
   * 
   * To explain this we'll need to review "Currying" (named after Haskell Curry).
   * Below is an example ocaml toplevel interaction
   * # let add a b = a + b;;
   * val add : int -> int -> int = <fun>
   * # add 3 4;;
   * - : int = 7
   * # (add 3) 4;;
   * - : int = 7
   *
   * What's happening above?
   * ocaml only uses functions with one parameter. functions with more than
   * one parameter are "currying" or chaining functions together.
   * 
   * The result of the function declaration can be seen below more clearly
   * let sum = fun i j -> i + j;;
   * is really
   * let sum = (fun i -> (fun j -> i + j));;
   *
   * So in the context below
   * List.map will execute (translate env) iterating over the list functions
   * The function call begins with env (env type) that is defined above. Reminder
   * 
   * env = BE VERY CAREFUL HERE. env is delcared twice. That's right. env
   * is used for a while with the defined value just under the translate
   * definition (yeah, that's right both translate and env are BOTH used twice).
   * I digress. env is later overwritten as in this example:
   * # let env = 3 in let env = 4 in evn;;
   * -: int = 4
   * 
   * functions = (the parameter that was passed in) and is a list of
   * Ast.func_decls that would look something like this
   *   [{Ast.fname = "main"; formals = []; locals = ["b"];
   *    body =
   *     [Ast.Expr (Ast.Assign ("b", Ast.Literal 42));
   *      Ast.Expr (Ast.Assign ("a", Ast.Call ("inc", [Ast.Id "b"])));
   *      Ast.Expr (Ast.Call ("print", [Ast.Id "a"]))]};
   *   {Ast.fname = "inc"; formals = ["x"]; locals = ["y"];
   *    body =
   *     [Ast.Expr (Ast.Assign ("y", Ast.Literal 1));
   *      Ast.Return (Ast.Binop (Ast.Id "x", Ast.Add, Ast.Id "y"))]}]
   *  
   * entry_function = (declared immediately above and discussed there)
   *
   * OCaml Reminder:
   * this operator ::
   * is used on the left hand side of sometimes to pattermatch between
   * head :: tail of a list with the head being one object. However
   * it is also used as the "cons" operator. Example:
   * 7 :: [5; 3];; (* Gives [7; 5; 3] *)
   *
   * The RESULT of the the below operation is (pseudocode)
   * func_bodies = [ [Jsr(1); Hlt] ; 
   *                 [(translate "env for main" "main function") ; 
   *                  (translate "env for inc" "inc function")] ]
   * 
   * 
   *
   *)
  let func_bodies = entry_function :: List.map (translate env) functions in

  (* Calculate function entry points by adding their lengths *)
  let (fun_offset_list, _) = List.fold_left
      (fun (l,i) f -> (i :: l, (i + List.length f))) ([],0) func_bodies in
  let func_offset = Array.of_list (List.rev fun_offset_list) in

  (*
   * this is the return value of the translate function
   * it is a prog type defined in bytecode.ml
   *
   * OCaml reminder. The below is essentailly constructing
   * an instance of a OCaml record with { } for instance
   * assigning the member num_globals = withThisValue 
   *
   *)
  { num_globals = List.length globals;
    (* Concatenate the compiled functions and replace the function
       indexes in Jsr statements with PC values *)
    text = Array.of_list (List.map (function
	Jsr i when i > 0 -> Jsr func_offset.(i)
      | _ as s -> s) (List.concat func_bodies))
  }
