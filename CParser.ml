
#load "str.cma"

type data_type =
	|Type_Int
;;


type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast	(* cond * if brach * else branch *)
| While of ast * ast
| Paren of ast
  
;;

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_Assign
 | Tok_Greater
 | Tok_Less
 | Tok_Equal
 | Tok_LParen
 | Tok_RParen
 | Tok_Semi
 | Tok_Main
 | Tok_LBrace
 | Tok_RBrace
 | Tok_Int 
 | Tok_Float
 | Tok_Sum
 | Tok_Mult
 | Tok_Pow
 | Tok_Print
 | Tok_If
 | Tok_Else
 | Tok_While
 | Tok_END
 
(* tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_equal = Str.regexp "=="
let re_semi = Str.regexp ";"
let re_int = Str.regexp "int"
let re_float = Str.regexp "float"
let re_printf = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_num = Str.regexp "[-]?[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
let re_add = Str.regexp "+"
let re_mult = Str.regexp "*"
let re_pow = Str.regexp "\\^"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_while = Str.regexp "while"


exception Lex_error of int
exception Parse_error of int ;;
exception IllegalExpression of string

let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_add s pos) then
       Tok_Sum::(tokenize' (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tokenize' (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tokenize' (pos+2) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tokenize' (pos+2) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tokenize' (pos+4) s)    
     else if (Str.string_match re_while s pos) then
       Tok_While::(tokenize' (pos+5) s)       
	else if (Str.string_match re_pow s pos) then
       Tok_Pow::(tokenize' (pos+1) s)
    else if (Str.string_match re_printf s pos) then
       Tok_Print::tokenize' (pos+6) s
    else if (Str.string_match re_lbrace s pos) then
       Tok_LBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_rbrace s pos) then
       Tok_RBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_assign s pos) then
       Tok_Assign::(tokenize' (pos+1) s)
    else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tokenize' (pos+1) s)
    else if (Str.string_match re_less s pos) then
       Tok_Less::(tokenize' (pos+1) s)
    else if (Str.string_match re_semi s pos) then
       Tok_Semi::(tokenize' (pos+1) s)
    else if (Str.string_match re_int s pos) then
       Tok_Int::(tokenize' (pos+3) s)
    else if (Str.string_match re_float s pos) then
       Tok_Float::(tokenize' (pos+5) s)
    else if (Str.string_match re_main s pos) then
       Tok_Main::(tokenize' (pos+4) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s
 
 
 (* C Grammar *)
 (* 
 
 basicType-> 'int'
  mainMethod-> basicType 'main' '(' ')' '{' methodBody '}'
  methodBody->(localDeclaration | statement)*
  localDeclaration->basicType ID ';'
  statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';'
  exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n') 



*)

(*----------------------------------------------------------
  function lookahead : token list -> (token * token list)
	Returns tuple of head of token list & tail of token list
*)

let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t)
;;        



(* -------------- Your Code Here ----------------------- *)

let match_t tok t_list = 
match t_list with
 h::t when h = tok -> t
| _  -> Printf.printf "%d\n\n" (List.length t_list); 
 raise (IllegalExpression "match_t") ;;

let rec parse_main l = 
let (a1, l1) = parse_basic l in 
	let n = match_t Tok_Main l1 in 
	let n2 = match_t Tok_LParen n in 
	let n3 = match_t Tok_RParen n2 in 
	let n4 = match_t Tok_LBrace n3 in 
	let (a2, l2) = parse_mbody n4 [] in 
	(* let n5 = match_t Tok_RBrace l2 in *)
	(Fun (Type_Int, "main", List [], a2), Tok_END)

and parse_basic l = 
let (a,b) = lookahead l in 
	  match a with
	  Tok_Int -> (* Basic_type -> int *)
	  let n = match_t (Tok_Int) l in
	  (Type_Int,n)
	  | _ -> failwith "parse_basic error"

and parse_mbody l a_list = 
let (a,b) = lookahead l in
	match a with
	Tok_Int -> (* Local Declaration *)
	
	let (a1, l1) = parse_ldec l in
	parse_mbody l1 (a_list@[a1])

	| Tok_While -> let (a2, l2) = parse_while l in 
	  parse_mbody l2 (a_list@[a2])

    | Tok_If -> let (a3, l3) = parse_if l in
      parse_mbody l3 (a_list@[a3])
    | Tok_Id id -> let (a4, l4) = parse_assign l in 
      parse_mbody l4 (a_list@[a4])
    | Tok_Print -> let (a5, l5) = parse_print l in 
      parse_mbody l5 (a_list@[a5])
    | Tok_END -> (List a_list, l)
    | Tok_RBrace -> (List a_list, b) 
    | _ ->  Printf.printf "%d\n\n" (List.length l); failwith "parse_mbody"

and parse_ldec l =
let (a,b) = lookahead l in 
	match a with
	
	Tok_Int -> ( let n = match_t Tok_Int l in 
	        let (c,d) = lookahead n in match c with

		Tok_Id a -> ( let n1 = match_t (Tok_Id a) n in 
			let n2 = match_t Tok_Semi n1 in 
			(Define (Type_Int,Id a),n2) )
		| _ -> failwith "parse_ldec"   )
	
	| _ -> failwith "parse_ldec"

and parse_statement l =
let (a,b) = lookahead l in
	match a with
	Tok_While -> parse_while l 
	| Tok_If -> parse_if l 
	| Tok_Id id -> parse_assign l 
	| Tok_Print -> parse_print l 
| _ -> failwith "parse_statement"

and parse_statements l a_list = 
let (a,b) = lookahead l in 
match a with
Tok_While -> let (a2,l2) = parse_while l in 
	parse_statements l2 (a_list@[a2])
	| Tok_If -> let (a2,l2) = parse_if l in
	parse_statements l2 (a_list@[a2])
	| Tok_Id id -> let (a2,l2) = parse_assign l in
	parse_statements l2 (a_list@[a2])
	| Tok_Print -> let (a2,l2) = parse_assign l in 
parse_statements l2 (a_list@[a2])
| Tok_RBrace -> (List a_list, l)
	| _ ->  Printf.printf "%d\n\n" (List.length l);
failwith "parse_statements"


and parse_while l =

	let (a,b) = lookahead l in 
	match a with
	
		Tok_While -> ( let n = match_t Tok_While l in 
		let n2 = match_t Tok_LParen n in 
		let (a2, b2) = parse_exp n2 in 
		let n3 = match_t Tok_RParen b2 in 
		let n4 = match_t Tok_LBrace n3 in 
		let (a3, b3) = parse_mbody n4 [] in 
		(* let n5 = match_t Tok_RBrace b3 in *)
		(While (a2,a3), b3))
	
	| _ -> failwith "parse_while"

and parse_if l =
let (a,b) = lookahead l in 
	match a with
	Tok_If -> ( let n = match_t Tok_If l in 
		let n2 = match_t Tok_LParen n in 
		let (a2,b2) = parse_exp n2 in (* Conditional *)
		let n3 = match_t Tok_RParen b2 in 
		let n4 = match_t Tok_LBrace n3 in 
	        let (a3,b3) = parse_mbody n4 [] in 
       
       
       
let (q1,q2) = lookahead b3 in match q1 with
|Tok_Else -> let (a4,b4) = parse_else b3 in
(If (a2,a3,a4), b4)
| _ -> (If (a2,a3, List[]), b3) )
        
		
	
	| _ -> failwith "parse_if"

and parse_else l = 
let (a,b) = lookahead l in
	match a with
	
	Tok_Else -> ( let n = match_t Tok_Else l in 
		let n2 = match_t Tok_LBrace n in 
		let (a2,b2) = parse_mbody n2 [] in 
	        
		(a2, b2) )
	
	| _ ->  failwith "parse_else"

and parse_assign l =
let (a,b) = lookahead l in
	match a with
	
	Tok_Id id ->  ( let n = match_t (Tok_Id id) l in 
		let n2 = match_t Tok_Assign n in 
		let (a2,b2) = parse_exp n2 in 
		let n3 = match_t Tok_Semi b2 in 
		(Assign (Id id, a2), n3) )
	
	| _ -> failwith "parse_assign"

and parse_print l =
let (a,b) = lookahead l in
match a with
	Tok_Print -> ( let n = match_t Tok_Print l in 
		let n2 = match_t Tok_LParen n in 
		let (a2,b2) = parse_exp n2 in 
		let n3 = match_t Tok_RParen b2 in 
		let n4 = match_t Tok_Semi n3 in 
		(Print a2, n4) )
| _ -> failwith "parse_print"

and parse_exp l = 

let (a,b) = parse_additive l in 
	let (c,d) = lookahead b in 
	match c with 
	Tok_Less -> let (a2,b2) = parse_exp d in (Less (a,a2), b2)
	| Tok_Equal -> let (a2,b2) = parse_exp d in (Equal (a,a2), b2)
	| Tok_Greater -> let (a2,b2) = parse_exp d in (Greater (a,a2), b2)
	| Tok_Sum -> let (a2,b2) = parse_exp d in (Sum (a,a2), b2)
	| Tok_Mult -> let (a2,b2) = parse_exp d in (Mult (a,a2), b2)
	| Tok_Pow -> let (a2,b2) = parse_exp d in (Pow (a,a2), b2)
	| _ -> (a,b)

and parse_additive l =
	let (a,b) = parse_mult l in 
	let (c,d) = lookahead b in 
	match c with
        Tok_Sum -> let (a2,b2) = parse_additive d in (Sum (a,a2), b2)
	| _ -> (a,b)

and parse_mult l =
let (a,b) = parse_pow l in 
	let (c,d) = lookahead b in 
	match c with
	Tok_Mult -> let (a2,b2) = parse_mult d in (Mult (a,a2), b2)
	| _ -> (a,b)

and parse_pow l = 
let (a,b) = parse_primary l in 
	let (c,d) = lookahead b in
         match c with 
	Tok_Pow -> let (a2,b2) = parse_pow d in (Pow (a,a2), b2)
	| _ -> (a,b)
and parse_primary l =
let (a,b) = lookahead l in 
	match a with
	Tok_LParen -> let (a2,b2) = parse_exp b in 
	       	let n = match_t Tok_RParen b2 in (Paren a2,n)
	| Tok_Id id -> (Id id, b)
	| Tok_Num a -> (Num a, b)
	| _ -> failwith "parse_primary"
;;



let parse_Function lst = parse_main lst
(* ------------------------------------------------------*)





exception Error of int ;;




let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let tok_to_str t = ( match t with
          Tok_Num v -> string_of_int v
        | Tok_Sum -> "+"
        | Tok_Mult ->  "*"
        | Tok_LParen -> "("
        | Tok_RParen -> ")"
		| Tok_Pow->"^"
        | Tok_END -> "END"
        | Tok_Id id->id
		| Tok_String s->s
		| Tok_Assign->"="
		 | Tok_Greater->">"
		 | Tok_Less->"<"
		 | Tok_Equal->"=="
		 | Tok_Semi->";"
		 | Tok_Main->"main"
		 | Tok_LBrace->"{"
		 | Tok_RBrace->"}"
		 | Tok_Int->"int" 
		 | Tok_Float->"float"
		 | Tok_Print->"printf"
		 | Tok_If->"if"
		 | Tok_Else->"else"
		 | Tok_While-> "while"
    )

let print_token_list tokens =
	print_string "Input token list = " ;
	List.iter (fun x -> print_string (" " ^ (tok_to_str x))) tokens;
	print_endline ""
;;
	




let rec print_ws pos = 
if pos > 0 then (Printf.printf "    "; print_ws (pos-1)) else () ;;

let rec pretty_print pos x =
match x with
Fun (a,b,c,d) -> Printf.printf "int main(){\n"; print_list 1 d; 
Printf.printf "\n}"; Printf.printf "\n"
| _ -> failwith "pretty_print" 

and print_ast pos x =
match x with

Id s -> print_ws pos; Printf.printf "%s" s

| Num a -> print_ws pos; Printf.printf "%d" a

| Define (a,b) ->
 print_ws pos; Printf.printf "int "; print_ast 0 b; Printf.printf ";"

| Assign (a,b) -> print_ws pos; print_ast 0 a;
Printf.printf " = "; print_ast 0 b; Printf.printf ";"

| List l -> print_list pos (List l)

(* Do Fun *)

| Sum (a,b) -> print_ws pos; print_ast 0 a;
Printf.printf " + "; print_ast 0 b; 

| Greater (a,b) -> print_ws pos; print_ast 0 a; Printf.printf " > ";
print_ast 0 b; 

| Equal (a,b) -> print_ws pos; print_ast 0 a; Printf.printf " == ";
print_ast 0 b; 

| Less (a,b) -> print_ws pos; print_ast 0 a; Printf.printf " < ";
print_ast 0 b; 

| Mult (a,b) -> print_ws pos; print_ast 0 a; Printf.printf " * ";
print_ast 0 b; 

| Pow (a,b) -> print_ws pos; print_ast 0 a; Printf.printf " ^ ";
print_ast 0 b; 

| Print a -> print_ws pos; Printf.printf "printf("; print_ast 0 a;
Printf.printf ");"

| If (a,b,c) -> print_ws pos; Printf.printf "if("; print_ast 0 a;
Printf.printf "){\n"; print_ast (pos+1) b; Printf.printf "\n";
print_ws pos;
(if c = (List []) then
              Printf.printf "}"
    else (Printf.printf "}else{\n"; 
print_ast (pos+1) c; Printf.printf "\n"; print_ws pos; Printf.printf "}"))

| While (a,b) -> print_ws pos; Printf.printf "while(";
print_ast 0 a; Printf.printf "){\n"; print_ast (pos+1) b;
Printf.printf "\n"; print_ws pos;
Printf.printf "}"

| Paren a -> print_ws pos; Printf.printf "("; print_ast 0 a;
Printf.printf ")"

| _ -> Printf.printf "not done"


and print_list pos x=
      match x with
 List [] -> ()
 | List (h::t) -> 
     print_ast pos h; 
( match t with
[] -> ()
| _ -> Printf.printf "\n"; print_list pos (List t) )

| _ -> Printf.printf "not done"
;;

(* ----------------------------------------------------- *)


