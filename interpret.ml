(*
 * bring in all the OCaml types declared in ast.ml
 *)
open Ast

(*
 * Background on OCaml "named strctures"
 * from Hickey's "Intro to OCaml"
 * Named structures are defined with the modeul and struct keywords
 * module name must be upper case. the implementation may
 * include anything that may occur in a .ml file
 *
 * essentially below is defining a mini class.
 *
 * Map.Make is producing a "balanced tree" implementation of a
 * ADT dictionary (key/value). The value that will be returned for
 * a valid key will be of the type defined in the struct...end
 *
 * the module name (in this case "NameMap") must be capitalized
 *
 * It appears as thought this NameMap is no different then a normal
 * map where string is the key and the value is yet to be defined
 *
 *)
module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

(*
 * TODO: ReturnException of tuple (int, int NameMap.t)
 * unknown how int NameMap.t works
 *)
exception ReturnException of int * int NameMap.t

(* 
 * run function that is the entry point for the program
 * microc will use this handle to start the interpreter
 * however the actual entry point is way down below
 * which calls the first function main
 *)
let run (vars, funcs) =
  (* Put function declarations in a symbol table *)
  let func_decls = List.fold_left
      (*
       * OCaml reminder:
       * List.fold_left f a [b1; ...; bn]
       * will produce f (...(f (f a b1) b2)...) bn
       *
       * so in this case we are working with
       * List.fold_left (function to add the functions to a NameMap) (empty NameMap) (list of functions)
       *
       * f   = function that takes in two arguments a (funcs) and b (fdecl)
       *       which is items from the list of functions declared
       * a   = growing Map. NameMap.add always takes a map argument and adds its key/value
       *       and returns a map. In fold_left this will be fed into the next iteration
       *       so NameMap.add will have the "a" continually updating with the new map
       * b_n = will an iteration through the funcs list of function declarations
       *
       * example:
       * (NameMap.add (Key: "main") (Value: <<main declaration info>>) current_map)
       * previous naming was pretty confusing and has been simplified below
       *
       * The end result is that we now have function declarations in a symbol
       * table
       *)
      (fun fmap fd_n -> NameMap.add fd_n.fname fd_n fmap) NameMap.empty funcs
  in (* end of let func_decls = List.fold_left *)


  (*
   * Now that we know all the global variables and we know
   * all the function declarations we are ready to call main
   * and potentially any other functions called within main.
   *
   * We begin by calling
   * call <<main fdecl>> [] globals
   *)
  (* Invoke a function and return an updated global symbol table *)
  let rec call fdecl actuals globals =

    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env = function
        (*
         * Following our example with hello2.mc
         * 
         * We've are currently executing eval with the following attributes
         * eval (locals = ["b": 0], globals = ["a": 0]) (Ast.Literal 42)
         *
         * We've arrived here because we've matched to literal
         * because nothing changes for the literal we can return the environment
         * exactly as it was received and we return the result of the Literal
         * i
         *)
	      Literal(i) -> i, env
      | Noexpr -> 1, env (* must be non-zero for the for loop predicate *)
      | Id(var) -> let locals, globals = env 
        in 
          if NameMap.mem var locals then (NameMap.find var locals), env 
          else if NameMap.mem var globals then (NameMap.find var globals), env 
          else raise (Failure ("undeclared identifier " ^ var))
      | Binop(e1, op, e2) -> 
          let v1, env = eval env e1 in 
          let v2, env = eval env e2 in 
          let boolean i = if i then 1 else 0 in
	        (match op with
	          Add -> v1 + v2
	        | Sub -> v1 - v2
	        | Mult -> v1 * v2
	        | Div -> v1 / v2
	        | Equal -> boolean (v1 = v2)
	        | Neq -> boolean (v1 != v2)
	        | Less -> boolean (v1 < v2)
	        | Leq -> boolean (v1 <= v2)
	        | Greater -> boolean (v1 > v2)
	        | Geq -> boolean (v1 >= v2)), env
      (*
       * Following our example we've now called eval env with
       * eval (locals = ["b": 0], globals = ["a": 0]) (Ast.Expr (Ast.Assign ("b", Ast.Literal 42)))
       *
       * We arrive at this match and set the following
       * var = "b"
       * e   = Ast.Literal 42
       *
       * let v, (locals, globals) = eval env e
       * will take the expression that is possible on the right side of the
       * assignment and work on it first. The result of this function call
       * will result in a return value that is fed to "v" the final value
       * and we will also receive an updated environment in the form (locals, globals)
       * See the above for that process
       *
       * Finally we arrive at
       * v = 42
       * (locals, globals) = whatever they may be. but in this example nothing has changed
       *
       * First we check if the variable in this assignment is in the locals by using
       * NameMap.mem. If it is we assign the value in the NameMap. And the return value is
       * the new locals NameMap and same globals NameMap.
       *
       * If we don't find it in the locals, but we find it in the globals we do the same procedure
       * in the globals and send back the environment with no change to the locals.
       *
       * If the variable name is not found then we raise an exception
       *
       *)
      | Assign(var, e) -> let v, (locals, globals) = eval env e in 
          if NameMap.mem var locals then v, (NameMap.add var v locals, globals) 
          else if NameMap.mem var globals then v, (locals, NameMap.add var v globals)
	        else raise (Failure ("undeclared identifier " ^ var))
      | Call("print", [e]) -> let v, env = eval env e in print_endline (string_of_int v); 0, env
      | Call(f, actuals) -> let fdecl = try NameMap.find f func_decls with Not_found -> raise (Failure ("undefined function " ^ f))
	      in
	        let actuals, env = List.fold_left (fun (actuals, env) actual -> let v, env = eval env actual in v :: actuals, env) ([], env) (List.rev actuals)
	        in
	          let (locals, globals) = env 
            in 
              try 
                let globals = call fdecl actuals globals 
                in 
                  0, (locals, globals) 
              with ReturnException(v, globals) -> v, (locals, globals)
    in (* end of let rec eval env = function *)

    (*
     * the exec function will be caleld with the arguments
     *
     * env = (locals, globals) where locals is constructed from the
     *       prodedures below and globals is passed in from the first
     *       call of main. Reminder that fdecl is holding the function
     *       definitions as passed in from the "call" but we don't need
     *       to worry about that for now.
     *
     * pattern_matching_argument = fdecl.body that is passed in 
     *                             from the call below some distance
     *                             below starting with
     *                             snd (list.fold_left exec... ) 
     *
     * The return value of this function will be an updated invironment
     * which consists of (locals, globals). 
     *
     * One Example for the hello2.mc program: 
     * exec 
     * (locals = ["b": 0], globals = ["a": 0]) 
     * body =
     *     [Ast.Expr (Ast.Assign ("b", Ast.Literal 42));
     *      Ast.Expr (Ast.Assign ("a", Ast.Call ("inc", [Ast.Id "b"])));
     *      Ast.Expr (Ast.Call ("print", [Ast.Id "a"]))]
     *
     *)
    (* Execute a statement and return an updated environment *)
    let rec exec env = function
        (*
         * Block as defined in ast.ml is a stmt list
         * a stmt is also an expression
         * therefore the body sent in to be pattern matched
         * is a block and will be matched here
         * 
         * Using fold_left perform this function again
         * using the environment we still have
         * on the list of statements one by one from
         * left to right
         * 
         * OCaml reminder:
         * List.fold_left f a [b1; ...; bn]
         * will produce f (...(f (f a b1) b2)...) bn
         *
         * What will be returned from each exec is an updated
         * environment therefore the "a" in the above OCaml reminder
         * will be updated appropriately for each new executation
         *)
	      Block(stmts) -> List.fold_left exec env stmts
        (*
         * Using the example above as a guide...
         * Our first expression is Ast.Assign ("b", Ast.Literal 42))
         * 
         * We've found a match and set the above expression to e
         * The return value comes after the ->
         *
         * let _, env = 
         * means that we would like to perform some procedure
         * after the equal sign that fits into a size-2 tuple
         * like (x, y). In this case however _ inidicates that we
         * are not interested in the result. So we will simply catch
         * env. The return value from the "eval" function is (result, (locals, globals))
         * When we finally reach the point where we are analyzing the
         * Expr below we are no longer interested in the return value of the expression
         * for instance if the source code was
         * b = 42;
         * we are not interested in the return value of b = 42; we are interested
         * only in the way that this expression has influenced the environment
         * which is (locals, globals) not the first item so we neglect it with _
         *
         * eval env e
         * calls the function in the far above code. In our example
         * It will be calling with the following arguments
         * eval (locals = ["b": 0], globals = ["a": 0]) (Ast.Expr (Ast.Assign ("b", Ast.Literal 42)))
         * 
         * in env
         * finally when (eval env e)'s result is set to 
         * _, env the "in env" portion takes the named variable
         * env and returns it
         * this is the new global state after executing the expression
         *
         * following our example when we come back from the eval call
         * our return value in the let will look like this
         *
         * env = (["b": 42], ["a": 0])
         *)
      | Expr(e) -> let _, env = eval env e in env
      | If(e, s1, s2) -> let v, env = eval env e in exec env (if v != 0 then s1 else s2)
      | While(e, s) -> let rec loop env = let v, env = eval env e in if v != 0 then loop (exec env s) else env in loop env
      | For(e1, e2, e3, s) -> let _, env = eval env e1 in let rec loop env = let v, env = eval env e2 in if v != 0 then let _, env = eval (exec env s) e3 in loop env else env in loop env
      | Return(e) -> let v, (locals, globals) = eval env e in raise (ReturnException(v, globals))
    in (* end of let rec exec env = function *)

    (*
     * OCaml Reminder:
     * List.fold_left2 f a [b1; ...; bn] [c1; ...; cn] is
     * f (... (f (f a b1 c1) b2 c2) ...) bn cn
     * Invalid_argument is raised if the two lists have different lengths
     *
     * Vocabulary refresher:
     * formal parameters are those for the callee function
     * add(int a, int b) { return a + b; }
     *
     * actual parameters are those for the caller function
     * int j = 7; add(j, 3);
     *
     * actuals   = passed in from the initial call
     * fdecl     = passed in from the initial call
     *
     * List.fold_left2 asks the anonymous function
     * to iterate through the list of formals for this function
     * and the list of actuals for this function
     *
     * Example fdecl.formals may look like this
     * Ast.formals = ["x"; "y"]
     *
     * Example actuals may look like this
     * [Ast.Literal 4; Ast.Literal 5]
     *
     * So by the end of this function we now have a NameMap named locals
     * where the key is the local identifier and the value is caller
     * actual passed in
     *
     * at this point we have the locals that came from the function
     * signature. but not locals from created inside the function body
     * we still need to put those together
     *
     * let locals = RENAMED to let arglocals =
     * to avoid confusion
     *)
    let arglocals =
      try List.fold_left2
	  (fun locals formal actual -> NameMap.add formal actual locals)
	  NameMap.empty fdecl.formals actuals
      with Invalid_argument(_) ->
	raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
    in (* end of let locals = *)

    (*
     * Now we compile the locals that exist in the fdecl body
     * We do this by using or arglocals name map as a starting point
     *
     * Notice that if there is a variable name in arglocals that has the same
     * key as one in the locals it will be overidden with 0
     *
     * Note that there is no check to see if a local in the function body
     * is already in the local symbol table. They are simply overridden
     *
     *)
    let locals = List.fold_left
	(fun lmap ld -> NameMap.add ld 0 lmap) arglocals fdecl.locals
    in (* end of let locals = List.fold_left *)

    (*
     * Finally after the below lines have made the initial call to
     * the "main" function we find our selves dropping down to here
     * This is a subsequent entry point
     *
     * here the return value is the second item in the tuple that
     * is returned from the call to (list.fold_left exec ...)
     *
     * globals    = passed in from the initial call for "main"
     * fdecl.body = is the list of expressions that came in
     *              from the "main" call.
     * locals     = comes from a processing immediately above
     *)
    snd (List.fold_left exec (locals, globals) fdecl.body)

  (* Run a program: initialize global variables to 0, find and run "main" *)
  in (* end of let rec call fdecl actuals globals = *)



  
  
  let globals = List.fold_left
     (*
      * List.fold_left f a [b1; ...;bn]
      * f (... (f (f a b1) b2) ...) bn
      *
      * This method of ingesting the global variable declarions is essentially
      * the same as the above method of ingesting the function declarations
      *
      * Starting off with an emtpy map and the vars list passed in from the
      * original function call we work from left to right in the list
      * adding each variable name with a value of 0 to the map.
      * eventually saving the results in globals.
      *
      * (fun gmap vd) renamed for clearity
      *)
     (fun gmap vd -> NameMap.add vd 0 gmap) NameMap.empty vars
  in (* end of let globals = List.fold_left *)
     try
     (*
      * Entry point into function "main"
      *
      * NameMap.find will lookup the key "main" in the map func_decls
      * And will return the func_decls if it si found
      *
      * if it is not found .find should return an exception
      * that will be caught and matched by with Not_found.
      * We raise our own exception named "Failure" with a message
      *
      * call takes the arguments fdecl actulas and globals
      * the main() in microc has no arguments/actuals
      * the globals we pass in is the result of the above
      *)
     call (NameMap.find "main" func_decls) [] globals
     with Not_found -> raise (Failure ("did not find the main() function"))
