type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Literal of int
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

(*
 * this is an OCaml record like a C Struct
 * fname is the identifier name and string 
 * is the type
 *)
type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

(*
 * This is declaring a type "program" that is
 * a tuple (i.e. (value1, value2)) of a
 * (list of strings, list of func_decls)
 *)
type program = string list * func_decl list

(*
 * end of the line
 * finally pattern matching and giving back the string as appropriate
 *)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

(*
 * Pattern match and find the type 
 * statements really encapsulate expressions ("Expr")
 * As an example when string_of_stmt is handed and expression it is then
 * sent to string_of_expr to be taken care of there.
 * still this pattern matcher is still responsible for adding
 * some syntactic niceties (the carriage return, spacing, the keywords
 * in human string form and semicolons)
 *)
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

(*
 * in microc the only variable declarations are for ints
 * we are adding the human niceties like the spacing, the semicolon
 * and the carriage return
 *)
let string_of_vdecl id = "int " ^ id ^ ";\n"

(*
 * Big payoff: essentially pull out the members of the fdecl (c-struct like)
 * object. spit out the function declaration like one normally sees it 
 * something lik this
 * 
 * function_name(var a, var b) 
 * { 
 * var c; 
 * print(c);
 * }
 *
 * With some helper functions string_of_vdecl and string_of_stmt this is done
 *
 * String.concat s1 strlst
 * will concatentate the items in the strlst inserting s1 inbetween
 * In this case we are sending it "" so that string_of_vdecl really takes over
 *
 * List.map will run the same function over the items in the list
 * [(string_of_vdecl fdecl.local_0) ; ... ; (string_of_vdecl fdecl.locals_n)]
 *)
let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n
  {\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*
 * Entry point called by microc -a
 *
 * Reminder:
 * List.map f [a1; ... ;an] = [f a1; ... ;f an]
 * So List.map will execute string_of_vdecl on the list vars
 *
 * String.concat seperator_string list_of_strings
 * Will return a string where of concatenated list_of_strings
 * with separator_string inserted between each string in that
 * list 
 * 
 * So essentially below the first line is combining 
 * Everything in the parentheses into one big string and appending
 * "\n" to the end of it. And then appending the second line
 *
 * The second line instead adds a "\n" inbetween each item in the list
 * returned by (List.map string_of_fdecl funcs)
 * 
 * The goal of this function is to print the parsed vars and funcs
 * both are a list that will be digested peice by piece.
 * vars = the global variables declared
 * funcs = the declared/defined functions and all their infor
 *
 * After printing a list of all the variable declarations
 * print the functions
 *)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
