let rec evaluate e exp =
match exp with
Num a -> a
| Id s -> Hashtbl.find e s
| Sum (a,b) -> let k = evaluate e a in
let k2 = evaluate e b in
if k = (-10) || k2 = (-10) then failwith "unassigned"
 else (evaluate e a) + (evaluate e b)
| Greater (a,b) -> let k = evaluate e a in
let k2 = evaluate e b in
if k = (-10) || k2 = (-10) then failwith "unassigned" else
if (evaluate e a) > (evaluate e b) then 1 else -1
| Equal (a,b) -> let k = evaluate e a in
let k2 = evaluate e b in
if k = (-10) || k2 = (-10) then failwith "unassigned" else
if (evaluate e a) = (evaluate e b) then 1 else -1
| Less (a,b) -> let k = evaluate e a in
let k2 = evaluate e b in
if k = (-10) || k2 = (-10) then failwith "unassigned" else
if (evaluate e a) < (evaluate e b) then 1 else -1
| Mult (a,b) -> let k = evaluate e b in
let k2 = evaluate e a in 
if k = (-10) then failwith "unassigned" else if
k2 = (-10) then failwith "unassigned" else (evaluate e a) * (evaluate e b)
| Pow (a,b) -> let k = evaluate e a in
let k2 = evaluate e b in
if k = (-10) || k2 = (-10) then failwith "unassigned" else
int_of_float (
float_of_int (evaluate e a) ** float_of_int (evaluate e b))
| Paren a -> evaluate e a
| _ -> failwith "evaluate"
;;
let rec evaluate_while env a b =
let check = evaluate env a in if check = 1 then let k = 
eval_h env b in evaluate_while env a b else env
(* if evaluate env a = 1 then eval_h env b else evaluate_while env a b;
evaluate_while env a b*)
and eval_h env l =
match l with
[] -> env
| h::t -> ( match h with
Define (a,Id s) ->   (Hashtbl.add env s (-10));  eval_h env t
| Assign (Id s,b) -> let q = Hashtbl.find env s in
(* if q = (-10) then failwith "Exception" else *)
let n = evaluate env b in (Hashtbl.add env s n);
eval_h env t
| Print a -> let q = evaluate env a in
Printf.printf "%d\n"  q; eval_h env t
| If (a,List b,List c) -> if (evaluate env a) = 1 then
eval_h env b else eval_h env c; eval_h env t
| While (a, List b) -> if (evaluate env a) != 1 then eval_h env t else
 evaluate_while env a b; eval_h env t
| Paren a -> ( match a with List k ->  eval_h env k; eval_h env t 
                | _ -> evaluate env a; eval_h env t))
| _ -> failwith "eval_h"
;;

let rec eval env x =
match x with
Fun (a,b,c,List d) -> eval_h env d
| _ -> failwith "beginning"
       
;;
