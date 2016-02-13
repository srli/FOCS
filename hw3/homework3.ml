(* 

HOMEWORK 3

Name: Sophia Li

Email: sophia.li@students.olin.edu

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
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

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = Bytes.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (Bytes.set str index c; loop(cs,index+1))
  in
    loop(cs,0)



(*
 *  The type of a finite automaton
 * 
 *  When the transition relation is a function
 *  (i.e., every p,a has q such that (p,a,q) is in 
 *  delta) then this is a deterministic finite automaton  
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               accepting : 'a list }




(* QUESTION 1 *)
let rec findTransitions_helper (deltas,q,a) = 
  match deltas with
  | [] -> []
  | (q1, a1, p1) :: tl ->
    if q1 = q then
      if a1 = a then
        (q1, a1, p1)::findTransitions_helper(tl, q, a)
      else
        findTransitions_helper(tl, q, a)
    else
      findTransitions_helper(tl, q, a)

let findTransitions (fa,q,a) = 
  findTransitions_helper(fa.delta, q, a)

let rec isAccepting_helper (a_states, s) = 
  match a_states with
  | [] -> false
  | hd :: tl ->
    if hd = s then
      true
    else
      isAccepting_helper(tl, s)

let isAccepting (fa,s) =
  isAccepting_helper(fa.accepting, s)

let step (fa,q,a) =
  match findTransitions(fa, q, a) with
  | [] -> failwith "no step found"
  | (q1, a1, p1) :: tl ->
      p1

let rec steps (fa,q,syms) = 
  match syms with
  | [] -> q
  | hd :: tl -> 
    steps(fa, step(fa, q, hd), tl)

let rec isDFA_helper2(fa, state, alphabet) = 
  match alphabet with
  | [] -> true
  | a_hd :: a_tl ->
    if List.length(findTransitions(fa, state, a_hd)) = 1 then
      isDFA_helper2(fa, state, a_tl)
    else
      false

let rec isDFA_helper(fa, states, alphabet) = 
  match states with
  | [] -> true
  | s_hd :: s_tl ->
    if isDFA_helper2(fa, s_hd, alphabet) then
      isDFA_helper(fa, s_tl, alphabet)
    else
      false

(* No transitions can have the same q and a *)
let isDFA (fa) =
  isDFA_helper(fa, fa.states, fa.alphabet)


let acceptDFA (fa,input) = 
  if isDFA(fa) then
    match explode(input) with
    | [] -> true
    | _ ->
      if isAccepting(fa, steps(fa, fa.start, explode(input))) then
        true
      else
        false
  else
    failwith "Not DFA"


(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY AUTOMATA *)
(* REPLACE BY YOUR OWN DEFINITIONS *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
      ("one",'a',"two");
      ("two",'a',"start");
      ("start",'b',"start");
      ("one",'b',"one");
      ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
} 

let dfa_q2_a = { 
  states = ["start"; "oneb"; "twob"; "fail"];
  alphabet = ['a';'b'];
  delta = [("start", 'a', "start");
          ("start", 'b', "oneb");
          ("oneb", 'a', "start");
          ("oneb", 'b', "twob");
          ("twob", 'a', "start");
          ("twob", 'b', "fail");
          ("fail", 'a', "fail");
          ("fail", 'b', "fail")];
  start = "start";
  accepting = ["start"; "oneb"; "twob"]
}


let dfa_q2_b = { 
  states = ["start"; "oneb"; "twob"; "threeb"; "onea"; "twoa"; "threea"; "fail"];
  alphabet = ['a';'b'];
  delta = [("start", 'a', "onea");
          ("start", 'b', "oneb");
          ("onea", 'a', "twoa");
          ("onea", 'b', "oneb");
          ("twoa", 'a', "threea");
          ("twoa", 'b', "oneb");
          ("threea", 'a', "fail");
          ("threea", 'b', "oneb");
          ("oneb", 'b', "twob");
          ("oneb", 'a', "onea");
          ("twob", 'b', "threeb");
          ("twob", 'a', "onea");
          ("threeb", 'b', "fail");
          ("threeb", 'a', "onea");
          ("fail", 'a', "fail");
          ("fail", 'b', "fail")];
  start = "start";
  accepting = ["start"; "oneb"; "twob"; "threeb"; "onea"; "twoa"; "threea"]
}


let dfa_q2_c = {
  states = ["start"; "oneb"; "twob"; "threeb"; "onea"; "twoa"; "threea"; "fail"];
  alphabet = ['a';'b'];
  delta = [("start", 'a', "onea");
          ("start", 'b', "oneb");
          ("onea", 'a', "twoa");
          ("onea", 'b', "fail");
          ("twoa", 'a', "threea");
          ("twoa", 'b', "fail");
          ("threea", 'a', "threea");
          ("threea", 'b', "oneb");
          ("oneb", 'b', "twob");
          ("oneb", 'a', "fail");
          ("twob", 'b', "threeb");
          ("twob", 'a', "fail");
          ("threeb", 'b', "threeb");
          ("threeb", 'a', "onea");
          ("fail", 'a', "fail");
          ("fail", 'b', "fail")];
  start = "start";
  accepting = ["threeb"; "threea"]
}


let nfa_q2_d = {
  states = ["start"; "success"; "onea"; "twoa"; "threea"; "failb"];
  alphabet = ['a';'b'];
  delta = [("start", 'b', "success");
          ("start", 'a', "onea");
          ("onea", 'b', "success");
          ("onea", 'a', "twoa");
          ("twoa", 'b', "success");
          ("twoa", 'a', "threea");
          ("threea", 'a', "threea");
          ("threea", 'b', "failb");
          ("failb", 'b', "failb");
          ("failb", 'a', "threea");
          ("success", 'a', "success");
          ("success", 'b', "success")];
  start = "start";
  accepting = ["success"; "onea"; "twoa"; "threea"]
}


(* QUESTION 3 *)

let rec setIn (e,xs) =
   match xs with
   | [] -> false
   | hd :: tl ->
      if hd = e then
         true
      else
         setIn(e, tl)

let rec filter (trs) = 
  match trs with
  | [] -> []
  | hd :: tl ->
    if setIn(hd, tl) = false then
      hd :: filter(tl)
    else
      filter(tl)

let rec keepTarget_helper (trs) = 
  match trs with
  | [] -> []
  | (q, a, p) :: tl ->
    p::keepTarget_helper(tl)

let keepTarget (trs) = 
  filter(keepTarget_helper(trs))

let rec isAcceptingAny (fa,qs) =
  match qs with
  | [] -> false
  | hd :: tl ->
    if isAccepting (fa, hd) then
      true
    else
      isAcceptingAny(fa, tl)

let rec stepAll (fa,qs,a) = 
  match qs with
  | [] -> []
  | hd :: tl ->
    keepTarget(findTransitions(fa, hd, a))@stepAll(fa, tl, a)

let rec stepsAll (fa,qs,syms) = 
  match syms with
  | [] -> qs
  | hd :: tl ->
    stepsAll(fa, stepAll(fa, qs, hd), tl)

(* let acceptDFA (fa,input) = 
  if isDFA(fa) then
    match explode(input) with
    | [] -> true
    | _ ->
      if isAccepting(fa, steps(fa, fa.start, explode(input))) then
        true
      else
        false
  else
    failwith "Not DFA" *)


let acceptNFA (fa,input) =
  match explode(input) with
  | [] -> true
  | _ ->
    if isAcceptingAny(fa, stepsAll(fa, [fa.start], explode(input))) then
      true
    else
      false

(* 
 * A sample DFA for testing
 *
 * It accepts the language of all strings over {a,b} with a
 * multiple-of-3 number of a's.
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
	    ("one",'a',"two");
	    ("two",'a',"start");
	    ("start",'b',"start");
	    ("one",'b',"one");
	    ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
} 



(* A sample NFA for testing
 *
 * It accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
 *)

let nfaLastThreeB = {
  states = [0;1;2;3];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
	    (0,'b',0);
	    (0,'c',0);
	    (0,'b',1);
	    (1,'b',2);
	    (2,'b',3); ];
  start = 0;
  accepting = [3]
} 


(* This function is the base function that langDFA and
 * langNFA use -- it basically loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 * *)


let langFA accept (fa,n) = 

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
  	  let ts = to_string fa.alphabet i  in
  	  let bound = expt (List.length fa.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept(fa,ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
    loop 0


(* 
 * Tester functions that dump the language accepted by a
 * finite automaton, either deterministic or not
 *
 *)
 
let langDFA x = langFA acceptDFA x
let langNFA x = langFA acceptNFA x

