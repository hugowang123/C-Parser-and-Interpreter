#use "CParser.ml";;
#use "CInterpreter.ml";;
if (Array.length Sys.argv) < 2 then raise (Error 101);;
let filename = Sys.argv.(1);;
let prg1 = read_lines filename;;
let code = List.fold_left (fun x y->x^y) "" prg1;;	
let t = tokenize code;;
let (a,b)=parse_Function t;;

let env = Hashtbl.create 10;;   
try
	eval env a  
with
	_ -> print_string "Exception"; env
;;

      
      

print_string "\n";

