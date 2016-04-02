(*

HOMEWORK 7

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
 * Do that in a _fresh_ OCaml shell
 * It has to load without any errors.
 *
 *)


(*
 * Type for grammars
 *
 *)

type grammar = {
  nonterminals: string list;
  terminals: string list;
  rules: (string * string) list;
  startsym : string
}


(*
 * Some sample (context-free) grammars
 *
 *)

let anbn = {
  nonterminals = ["S"];
  terminals = ["a";"b"];
  rules = [("S","");
           ("S","aSb")];
  startsym = "S"
}

let anbm = {
  nonterminals = ["S";"T";"B"];
  terminals = ["a";"b"];
  rules = [ ("S","TB");
            ("T","");
	    ("T","aTb");
	    ("B","");
	    ("B","Bb")];
  startsym = "S"
}


(*
 * Here's a grammar that is _not_ context-free
 *
 * It's also harder to generate its strings
 *
 *)

let anbncn = {
  nonterminals = ["S";"A";"B";"C";"X"];
  terminals = ["a";"b";"c"];
  rules = [ ("S","");
            ("S","ABC");
	    ("bX","Xb");
	    ("AX","a");
	    ("aX","Xa");
	    ("XC","c");
	    ("Xc","cX");
	    ("B","XbBX");
	    ("B","");
	    ("A","AA");
	    ("C","CC")];
  startsym = "S"
}




(* abbreviations *)

let map = List.map
let len = String.length
let sub = String.sub


(*
 * Utility functions
 *
 *)


(* check is lhs is a prefix of str *)

let prefix lhs str =
  lhs = (sub str 0 (len lhs))


(* replace prefix lhs of str with rhs *)

let replace lhs str rhs =
  let l = len lhs in
  rhs ^ (sub str l (len str - l))


(* try to apply rule (lhs,rhs) to str (assuming prefix prf) *)

let apply_rule prf (lhs,rhs) str =
  if len str < len lhs
    then []
  else if prefix lhs str
    then [prf^(replace lhs str rhs)]
  else []


(* try to apply every rule in rs to str *)

let rec apply_rules rs str =
  let rec loop prefix str =
    if str = "" then []
    else let rest = loop (prefix^(sub str 0 1)) (sub str 1 (len str -1))  in
       (List.fold_left (fun res r -> res@(apply_rule prefix r str)) [] rs)@rest  in
  loop "" str


(*
 * Perform an iteratively deepening depth-first search of the rewrite
 * tree
 *
 *)

module StringSet = Set.Make(String)

let dfs_path maxdepth maxwidth grammar target =
  let lt = len target  in
  let rec loop q seen =
    if q = []
      then []
    else let ((path,d)::q) = q in
         let (str::_) = path in
	 if len str > maxwidth
	   then loop q seen
         else if len str = lt && str = target
	   then path
	 else if StringSet.mem str seen
	   then loop q seen
	 else if d > maxdepth
	   then loop q (StringSet.add str seen)
	 else (* let _ = (print_string str; print_newline()) in *)
	      let new_strs = apply_rules grammar.rules str in
	      let new_strs_d = map (fun x -> (x::path,d+1)) new_strs in
	      let q = (new_strs_d)@q in
	      loop q (StringSet.add str seen) in
  loop [([grammar.startsym],0)] StringSet.empty

let idfs_path maxdepth grammar target =
  let rec loop n =
    let _ = Printf.printf "Searching (depth %02d, max width %d)" n n in
    let _ = print_newline ()  in
    if n > maxdepth
      then []
    else match dfs_path n n grammar target with
         | [] -> loop (n+1)
	 | path -> path  in
  loop 1


(*
 * Check if a grammar is well-formed
 *
 *)

let checkGrammar grammar =
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.nonterminals  in
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.terminals  in
  let _ = List.iter (fun (p,q) -> if String.length p < 1 then failwith "rule with empty left-hand side" else ()) grammar.rules  in
  let _ = if List.mem grammar.startsym grammar.nonterminals then () else failwith "start symbol not a nonterminal"  in
  ()



(*
 * Try to generate a string for a given grammar
 *
 *)

let generate md grammar str =
  let _ = checkGrammar grammar in
  let print pre str = (print_string pre;
                       print_string str;
		       print_newline ())  in
  let rec rev_print path =
    match path with
    | [] -> ()
    | [s] -> print "   " s
    | s::ss -> (rev_print ss; print "-> " s)  in
  let path = idfs_path md grammar str  in
  let _ = rev_print path  in
  path != []



(*
 * QUESTION 1
 *
 *)

let palindromes = {
  nonterminals = ["S"];
  terminals = ["a";"b";"c"];
  rules = [("S", "");
          ("S", "a");
          ("S", "b");
          ("S", "c");
          ("S", "aSa");
          ("S", "bSb");
          ("S", "cSc")];
  startsym = "S"
}


let ambncmn = {
  nonterminals = ["A"; "B"];
  terminals = ["a"; "b"; "c"];
  rules = [("A", "");
          ("A", "aAc");
          ("A", "bBc");
          ("B", "");
          ("B", "Bbc")];
  startsym = "A"
}


let amcmnbn = {
  nonterminals = ["S"; "A"; "B"];
  terminals = ["a"; "b"; "c"];
  rules = [("S", "AB");
          ("A", "");
          ("A", "aAc");
          ("B", "");
          ("B", "cBb")];
  startsym = "S"
}


let ambncm = {
  nonterminals = ["A"; "B"];
  terminals = ["a"; "b"; "c"];
  rules = [("A", "");
          ("A", "aAc");
          ("A", "B");
          ("B", "");
          ("B", "bB")];
  startsym = "A"
}


let eqnum = {
  nonterminals = ["S"];
  terminals = ["d"; "e"];
  rules = [("S", "");
          ("S", "dSe")];
  startsym = "S"
}



(*
 * QUESTION 2
 *
 *)

(* Type for DFAs *)

type 'a dfa = { states: 'a list;
		alphabet: char list;
		delta: 'a -> char -> 'a;
		start : 'a;
		accepting : 'a list }


(* A dfa that accepts all strings with a multiple of three
 * number of as *)

let dfaThreeA = {
  states = ["S";"1";"2"];
  alphabet = ['a';'b'];
  delta = (fun q a ->
             match (q,a) with
	       ("S",'a') -> "1"
	     | ("1",'a') -> "2"
	     | ("2",'a') -> "S"
	     | ("S",'b') -> "S"
	     | ("1",'b') -> "1"
	     | ("2",'b') -> "2"
	     | _ -> "");
  start = "S";
  accepting = ["S"]
}

(* GRAMMER looks like this: *)
(* let eqnum = {
  nonterminals = ["S"];
  terminals = ["d"; "e"];
  rules = [("S", "");
          ("S", "dSe")];
  startsym = "S"
} *)

let all_pairings xs ys = List.fold_right (fun xs_hd xs_tl -> (List.fold_right (fun ys_hd ys_tl -> (xs_hd, ys_hd)::ys_tl) ys [])@xs_tl) xs []

let get_1 (a, _) = a
let get_2 (_, a) = a

let ($^) c s = Char.escaped c ^ s (* prepend *)

let rec char2String xs =
  match xs with
  | [] -> []
  | hd :: tl ->
    [Char.escaped hd] @ (char2String tl)

let rec dfaHelper dfa deltas =
  match deltas with
  | [] -> []
  | hd :: tl ->
    let retDelta = dfa.delta (get_1 hd) (get_2 hd) in
      [(get_1 hd), ($^) (get_2 hd) retDelta]@(dfaHelper dfa tl)

let rec dfaTerminals xs =
  match xs with
  | [] -> []
  | hd :: tl ->
    [(hd, "")] @ (dfaTerminals tl)

let dfaGrammar dfa =
  { nonterminals = dfa.states;
    terminals = char2String dfa.alphabet;
    rules = (dfaHelper dfa (all_pairings dfa.states dfa.alphabet)) @ (dfaTerminals dfa.accepting);
    startsym = dfa.start
  }



(*
 * QUESTION 3
 *
 *)
let addition = {
    nonterminals = ["A"; "B"; "C"];
    terminals = ["+"; "="; "x"];
    rules = [("A", "+=");
            ("A", "");
            ("A", "xAx");
            ("A", "+B");
            ("B", "");
            ("B", "x=x");
            ("B", "xBC");
            ("C=", "=C");
            ("C", "x")];
    startsym = "A"
  }

(* Test Results:
generate 12 addition "xx+xxx=xxxxx";;
Searching (depth 01, max width 1)
Searching (depth 02, max width 2)
Searching (depth 03, max width 3)
Searching (depth 04, max width 4)
Searching (depth 05, max width 5)
Searching (depth 06, max width 6)
Searching (depth 07, max width 7)
Searching (depth 08, max width 8)
Searching (depth 09, max width 9)
Searching (depth 10, max width 10)
Searching (depth 11, max width 11)
Searching (depth 12, max width 12)
   A
-> xAx
-> xxAxx
-> xx+Bxx
-> xx+xBCxx
-> xx+xxBCCxx
-> xx+xxx=xCCxx
-> xx+xxx=xxCxx
-> xx+xxx=xxxxx
- : bool = true

---

generate 12 addition "xxx+xx=xxxxx";;
Searching (depth 01, max width 1)
Searching (depth 02, max width 2)
Searching (depth 03, max width 3)
Searching (depth 04, max width 4)
Searching (depth 05, max width 5)
Searching (depth 06, max width 6)
Searching (depth 07, max width 7)
Searching (depth 08, max width 8)
Searching (depth 09, max width 9)
Searching (depth 10, max width 10)
Searching (depth 11, max width 11)
Searching (depth 12, max width 12)
   A
-> xAx
-> xxAxx
-> xxxAxxx
-> xxx+Bxxx
-> xxx+xBCxxx
-> xxx+xx=xCxxx
-> xxx+xx=xxxxx
- : bool = true
*)

let powers2 = {
  nonterminals = ["S"; "A"; "B"; "["; "]"; "#"; "L"];
  terminals = ["a"];
  rules = [("S", "#[A]L");
          ("A[A]", "[A]BB");
          ("#[A]", "[#]BB");
          ("[#]BB", "#AA[B]");
          ("[B]B", "AA[B]");
          ("[B]L", "AA[L]");
          ("AA[L]", "[A]BBL");
          ("L", "");
          ("[", "");
          ("]", "");
          ("#", "");
          ("A", "a");
          ("B", "a")];
  startsym = "S"
}
