(*************************************************** 

HOMEWORK 4

Name: Sophia Li

Email: sophia.li@students.olin.edu

Remarks, if any:

***************************************************)



(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)


(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.get str index)::result) in
  acc (String.length(str)-1) []

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a DETERMINISTIC finite automaton
 * 
 *  Note that the delta here is _actually_ a function
 *  from states and symbols to states
 * 
 *)

type 'a dfa = { states: 'a list;
		alphabet: char list;
		delta: 'a -> char -> 'a;
		start : 'a;
		accepting : 'a list }


(*
 *  A sample DFA that accepts all strings over
 *  {a,b} with a multiple-of-3 number of a's
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = (fun q a -> 
             match (q,a) with
	       ("start",'a') -> "one"
	     | ("one",'a') -> "two"
	     | ("two",'a') -> "start"
	     | ("start",'b') -> "start"
	     | ("one",'b') -> "one"
	     | ("two",'b') -> "two");
  start = "start";
  accepting = ["start"]
} 


(* QUESTION 1 *)
let rec isAccepting_helper a_states s = 
  match a_states with
  | [] -> false
  | hd :: tl ->
    if hd = s then
      true
    else
      isAccepting_helper tl s

let isAccepting dfa s =
    isAccepting_helper dfa.accepting s

let rec steps dfa q syms = 
  match syms with
  | [] -> q
  | hd :: tl -> 
    steps dfa (dfa.delta q hd) tl


let acceptDFA dfa input = 
  match explode input with
  | [] -> true
  | _ ->
    if isAccepting dfa (steps dfa dfa.start (explode input)) then
      true
    else
      false


(* This function loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is basically the same as in the last homework
 *)

let langDFA dfa n = 
  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  if n < 0 then ()
  else
    let print_str s = if s = "" then print_string "  <epsilon>\n"
                      else print_string ("  "^s^"\n")  in
    let rec loop i = 
      if i <= n then 
	let ts = to_string dfa.alphabet i  in
  	let bound = expt (List.length dfa.alphabet) i in
  	let rec loop2 j = 
  	  if j < bound then (if acceptDFA dfa (ts j) 
                               then print_str (ts j)
                             else ();
  			     loop2 (j+1))
  	  else ()  in
  	(loop2 0; loop (i+1))
      else ()  in
      loop 0


(* QUESTION 2 *)

let at_least n p xs =
  if List.length(List.filter p xs) >= n then
    true
  else
    false

let max_positive xs =  
  List.fold_right (fun x y -> if x > y then x else y) xs 0

let dbl x = "double of "^(string_of_int x);;
let neg x = "negation of "^(string_of_int x);;

let map_funs fs x = 
  List.fold_right (fun f1 f2 -> (f1 x)::f2) fs []

let map_cross fs xs =
  List.fold_right (fun x y -> (map_funs fs x)@y) xs []

let all_pairings xs ys =
  List.fold_right (fun xs_hd xs_tl -> (List.fold_right (fun ys_hd ys_tl -> (xs_hd, ys_hd)::ys_tl) ys [])@xs_tl) xs []


(* QUESTION 3 *)
let prefixes xs = 
  List.rev(List.fold_left (fun base x -> ((List.hd base)@[x])::base) [[]] xs)

let suffixes xs = 
  List.fold_right (fun x base -> ([x]@(List.hd base))::base) xs [[]]

(* List.fold_right (fun pref_x base -> (fun suffixes -> (List.hd suffixes)::pref_x) SUFFS HERE) (prefixes(xs)) [[]]
List.fold_right (fun xs_hd xs_tl -> (List.fold_right (fun ys_hd ys_tl -> (xs_hd, ys_hd)::ys_tl) ys [])@xs_tl) xs []
List.fold_right (fun pref_x base -> (fun suffixes -> (List.hd suffixes)::pref_x) (suffixes(xs))) (prefixes(xs)) [[]]
 *)
(* List.fold_right (fun pref_x base -> (List.fold_right (fun suff_x base-> suff_x) (List.map (fun suf_x -> a::suf_x) (suffixes(xs))) [])::base) (prefixes(xs)) [] *)
(* List.fold_right (fun suff_x base -> ((List.fold_right (fun pref_x base-> pref_x) (prefixes(xs)) [])@suff_x)::base) (List.map (fun suf_x -> a::suf_x) (suffixes(xs))) [] *)


let inject a xs =
  List.fold_right (fun suff_x base -> (List.hd prefixes(xs))@suff_x::base) (List.map (fun suf_x -> a::suf_x) (suffixes(xs))) []





(* List.map (fun suf_x -> a::suf_x) (suffixes(xs)) *)






  (* List.combine (List.map (fun pre_x -> pre_x::a) (prefixes(xs))) (suffixes(xs)) *)

(* let permutations xs =  failwith "permutations not implemented" *)
















