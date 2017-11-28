open Printf;;
open Stream;;

exception ParseError of string;;
exception ParseFailure of string;;
exception ParseOK;;
exception ParseEOF;;


(***********************************************************)
(* type declarations                                       *)
(***********************************************************)

type sign = 
    Plus
  | Minus
;;

type atom = 
    T
  | NIL
  | Int of int
  | Ident of string
;;

type token = 
    Lparen
  | Rparen
  | Dot
  | Sign of sign
  | Atom of atom
;;

type sexp = 
    AtomExp of atom
  | Sexp of sexp * sexp
;;


(***********************************************************)
(* globals                                                 *)
(***********************************************************)
let lineno = ref 1;;
let top_level = ref true;;


(***********************************************************)
(* printing functions                                      *)
(***********************************************************)

(* function: print_tokens - prints out a token stream  *)
let rec print_tokens ts = 
  match ts with parser
    [< 'Lparen; _ >] -> printf("Lparen "); print_tokens ts
  | [< 'Rparen; _ >] -> printf("Rparen "); print_tokens ts
  | [< 'Dot; _ >]    -> printf("Dot ");    print_tokens ts
  | [< 'Sign s; _ >] -> (match s with
      Plus  -> printf("Plus ");            print_tokens ts
    | Minus -> printf("Minus ");           print_tokens ts)
  | [< 'Atom a; _ >] -> (match a with
      T       ->   printf("Atom(T) ");     print_tokens ts
    | NIL     ->   printf("Atom(NIL) ");   print_tokens ts
    | Int n   ->   printf("Atom(Int(%d)) ") n;     print_tokens ts
    | Ident s ->   printf("Atom(Ident(%s)) ") s;   print_tokens ts)

  | [< >] -> printf("\n")
;;


(* function: string_of_op -  converts an operator token to a string *)
let string_of_op o =
  match o with
    Plus -> "+"
  | Minus -> "-"
;;

(* function: is_list - predicate function returning true if s-expression is a list *)
let rec is_list s = 
  match s with
    Sexp(h, AtomExp(NIL)) -> true
  | Sexp(h, t) -> is_list t
  | _ -> false
;;

(* function: string_of_atom - converts a primitive atom to a string *)
let string_of_atom a =
	match a with 
	T -> "t"
	|NIL -> "NIL"
	|Int(a) -> string_of_int a
	|Ident(a) -> a
	
	
;;

(* function: string_of_token - converts a lexer token to a string *)
let string_of_token t = 
	match t with 
	Lparen -> "("
	|Rparen -> ")"
	|Dot -> "."
	|Atom(a) -> string_of_atom a
	|Sign(a) -> string_of_op a

;;


(* function: print_list - prints an s-expression in list format *)
let rec print_list s =
	match s with
	  AtomExp(NIL) -> ()
	| AtomExp(a) -> printf("%s") (string_of_atom a);
	| Sexp(h,t) ->  print_sexp h;printf(""); print_list t
	
	 
	
			
		
		

(* function: print_sexp - prints an s-expression in either dotted or list format *)
and print_sexp s = 
	if(is_list s)then
	(printf("("); 
	print_list s;
	printf(")"))
	else 
	match s with 
	Sexp(h,t)-> printf("(");print_sexp h ; printf(".");print_sexp t;printf(")")
	|AtomExp(a)-> printf("%s") (string_of_atom a)
	
;;


(***********************************************************)
(* lexer implementation                                    *)
(***********************************************************)

let rec spaces s = 
  match s with parser
    [< '' '|'\t'|'\n' ; _ >] -> spaces s
  | [< >] -> ()
;;

let rec lexid str s = 
  match s with parser
    [< ''a'..'z'|'A'..'Z'|'0'..'9' as c ; _ >] -> lexid (str ^ (Char.escaped c)) s
  | [< >] -> str
;;



let rec lexint v s = 
match s with parser
  [< ''0'..'9' as n;_ >] -> (lexint (10 * v + (int_of_string(Char.escaped n))) (s));
| [< >] -> v
;;


let rec lexer s =
  match s with parser
    [< ''(' ; _ >] -> [< 'Lparen ; lexer s >]
  | [< '')' ; _ >] -> [< 'Rparen ; lexer s >]
  | [< ''.' ; _ >] -> [< 'Dot    ; lexer s >]
  | [< ''-' ; _ >] -> [< 'Sign(Minus) ; lexer s >]
  | [< ''+' ; _ >] -> [< 'Sign(Plus) ; lexer s >]
  | [< ''a'..'z'|'A'..'Z' as c; _ >] -> [< 'Atom(Ident(lexid (Char.escaped c) s)); lexer s >]
  | [< ''0'..'9' as n; _ >] -> [< 'Atom(Int(lexint (int_of_string (Char.escaped n)) s)); lexer s >]
  | [< '' '|'\t'|'\n'; _ >] -> spaces s; [< lexer s >]
  | [< >] -> [< >]
;;

(***********************************************************)
(* parser implementation                                   *)
(***********************************************************)

(* function: eat - consumes next value in token stream *)
let eat ts = try next ts; () with Stream.Failure -> ();;

(* function: check_sign - both validates and combines sign and integer token pairs *)
let check_sign s ts = 
  let lookahead = peek ts in
  match lookahead with 
    Some t -> 
      (match t with
	Atom a -> 
	  (match a with
	    Int a -> 
	      if (s=Minus) then
		(eat ts; AtomExp(Int(-a)))
	      else
		(eat ts; AtomExp(Int(a)))
	  | _ -> raise (ParseError("+/- sign may only be used with integer literals")))
      | _ -> raise (ParseError("+/- sign may only be used with integer literals")))
  | None -> raise (ParseError("EOF found before complete parsing of integer literal"))
;;


(* function: check_atom - converts identifiers to internal primitives, nested defun check *)
let check_atom a = 
  match a with
    Ident i -> (
      match i with
	"T" | "t" -> AtomExp(T)
      | "NIL" | "nil" -> AtomExp(NIL)
      | "DEFUN" | "defun" -> 
	  if !top_level = true then
	    AtomExp(a)
	  else
	    raise (ParseError("DEFUN used outside of top-level"))
      | _ -> AtomExp(a))
  | _ -> AtomExp(a)
;;



(* function: parse_sexp - top-level parser: takes stream of tokens, returns sexp-tree *)
(* S ::= E *)
let rec parse_sexp ts = 
  try
    top_level := true;
    let lookahead = (peek ts) in
    match lookahead with
      Some cur -> parse_exp ts
    | None -> raise ParseOK
  with 
    Stream.Failure -> raise ParseOK
  | ParseFailure msg -> raise (ParseError(msg))
  | ParseEOF -> raise (ParseError("found EOF before complete s-expression parsing"))

(* E ::= atom | '(' X          *)
and parse_exp ts =	

	let lookahead = (peek ts) in 
	match lookahead with 
	Some token -> (
		match token with 
		Sign(a) -> eat ts;check_sign a ts
		|Atom(a) ->  eat ts; check_atom a 
		|Lparen -> eat ts; parse_x ts
		|_ -> raise(ParseError("either you have an empty list or parsing is done"))
		)
	|None -> raise (ParseError("parse ended, expected atom or lparen"))
	

	
(* X ::= E Y  ')' | ')'   *)
and parse_x ts =

	let lookahead = (peek ts) in
	match lookahead with 
	Some token -> (
		match token with 
		|Rparen -> AtomExp(NIL)	
		|_ -> let a = parse_exp ts in 
	             let y = parse_y ts in 
			 parse_rparen ts;
			 Sexp(a,y)
		)
	|None -> raise (ParseError("there is only one atom"))
	
	

	


(* Y ::= '.' E | R     *)
and parse_y ts =
	
	let lookahead = (peek ts) in
	match lookahead with
		Some token -> (
		match token with 
		
		Dot -> eat ts;parse_exp ts 
		
		|_ -> parse_r ts )
	|None -> raise (ParseError("this is an sexp"))
	
	
(* R ::= E R | empty  *)
and parse_r ts = 

	let lookahead = (peek ts) in 
	match lookahead with 
		Some token -> (
		match token with 
		Atom(_)|Lparen ->( 
			let e =  parse_exp ts in 
			let r = parse_r ts in
			Sexp(e,r))
			
		|_-> AtomExp(NIL))
		
	|None -> raise (ParseError("not sure what to put here"))
	
	

(* convenience production for right parens *)
and parse_rparen ts = 
	 
	let lookahead = (peek ts) in 
	match lookahead with
	Some token -> (
		match token with 
		Rparen -> eat ts		
		|_-> raise(ParseError("rparen match anything error")) )
		
	|None -> raise (ParseError("somethings critically wrong none error"))
	
;;      
	


(*****************************************)
(* helper routines                       *)
(*****************************************)

(* function: get_sexp - wrapper function around parser to catch stream failures *)
let get_sexp s =
  try
    let sexplist = parse_sexp s in
    sexplist
  with 
    Stream.Failure -> AtomExp(NIL)
;;

(* function: next_sexp - recursive function which tries to evaluate as much as possible *)
let rec next_sexp ts = 
  try 
    let e = (get_sexp ts) in
    print_sexp e; printf("\n"); next_sexp ts
  with 
    ParseError e -> printf("Parse Error: %s\n") e
  | ParseOK -> exit 0
;;


(*****************************************)
(* main                                  *)
(*****************************************)
let ts = lexer (Stream.of_channel stdin) in
try 
 (print_sexp(get_sexp(ts));
  printf("\n");
  next_sexp ts)
with
  ParseError e -> printf("Parse Error: %s\n") e
| ParseOK -> exit 0
;;
