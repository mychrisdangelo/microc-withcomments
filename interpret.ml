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

  (* Invoke a function and return an updated global symbol table *)
  let rec call fdecl actuals globals =

    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env = function
	Literal(i) -> i, env
      | Noexpr -> 1, env (* must be non-zero for the for loop predicate *)
      | Id(var) ->
	  let locals, globals = env in
	  if NameMap.mem var locals then
	    (NameMap.find var locals), env
	  else if NameMap.mem var globals then
	    (NameMap.find var globals), env
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
      | Assign(var, e) ->
	  let v, (locals, globals) = eval env e in
	  if NameMap.mem var locals then
	    v, (NameMap.add var v locals, globals)
	  else if NameMap.mem var globals then
	    v, (locals, NameMap.add var v globals)
	  else raise (Failure ("undeclared identifier " ^ var))
      | Call("print", [e]) ->
	  let v, env = eval env e in
	  print_endline (string_of_int v);
	  0, env
      | Call(f, actuals) ->
	  let fdecl =
	    try NameMap.find f func_decls
	    with Not_found -> raise (Failure ("undefined function " ^ f))
	  in
	  let actuals, env = List.fold_left
	      (fun (actuals, env) actual ->
		let v, env = eval env actual in v :: actuals, env)
   	      ([], env) (List.rev actuals)
	  in
	  let (locals, globals) = env in
	  try
	    let globals = call fdecl actuals globals
	    in 0, (locals, globals)
	  with ReturnException(v, globals) -> v, (locals, globals)
    in (* end of let rec eval env = function *)

    (* Execute a statement and return an updated environment *)
    let rec exec env = function
	Block(stmts) -> List.fold_left exec env stmts
      | Expr(e) -> let _, env = eval env e in env
      | If(e, s1, s2) ->
	  let v, env = eval env e in
	  exec env (if v != 0 then s1 else s2)
      | While(e, s) ->
	  let rec loop env =
	    let v, env = eval env e in
	    if v != 0 then loop (exec env s) else env
	  in loop env
      | For(e1, e2, e3, s) ->
	  let _, env = eval env e1 in
	  let rec loop env =
	    let v, env = eval env e2 in
	    if v != 0 then
	      let _, env = eval (exec env s) e3 in
	      loop env
	    else
	      env
	  in loop env
      | Return(e) ->
	  let v, (locals, globals) = eval env e in
	  raise (ReturnException(v, globals))
    in (* end of let rec exec env = function *)

    (* Enter the function: bind actual values to formal arguments *)
    let locals =
      try List.fold_left2
	  (fun locals formal actual -> NameMap.add formal actual locals)
	  NameMap.empty fdecl.formals actuals
      with Invalid_argument(_) ->
	raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
    in (* end of let locals = *)


    (* Initialize local variables to 0 *)
    let locals = List.fold_left
	(fun locals local -> NameMap.add local 0 locals) locals fdecl.locals
    in (* end of let locals = List.fold_left *)

    (* Execute each statement in sequence, return updated global symbol table *)
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
