(* 

HOMEWORK 6

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
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *
 *)

type symbol = string

type 'a tm = { states : 'a list;
	       input_alphabet : symbol list;
	       tape_alphabet : symbol list;
	       left_marker : symbol;
	       blank : symbol;
	       delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
	       start : 'a;
	       accept : 'a;
	       reject : 'a }

type 'a config = { state : 'a;
		   before: symbol list;
		   after: symbol list }


(*
 *   Code to run a string tm machine
 *
 *)

let run m w = 

  let printConfig m config value = 
    let mw = 
      List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = 
      print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	       print_syms v') in
    let _ = print_newline ()  in
    value  in

  let acceptConfig m config = (config.state=m.accept) in

  let rejectConfig m config = (config.state=m.reject) in

  let haltConfig m c = (acceptConfig m c) || (rejectConfig m c) in

  let startConfig m w = 
    { state=m.start;before = [];after = m.left_marker::(explode w)} in

  let rec last u = 
    match u with
    | [] -> failwith "Moving Left from leftmost tape position"
    | [a] -> ([],a)
    | x::xs -> let (u',r) = last xs  in (x::u',r)   in

  let step m config = 
    if (haltConfig m config) then config
    else let (a,v') = match config.after with
                      | [] -> (m.blank,[])
		      | a::v' -> (a,v')  in
         let (q',b,dir) = m.delta(config.state,a) in
	 if dir = 0  (* left *)
	 then let (u',c) = last config.before in 
  	      {state=q';before=u';after=c::b::v'}
	 else {state=q';before=config.before@[b];after=v'} in

  let rec loop c = 
    let _ = printConfig m c c in
    if  (acceptConfig m c) then true
    else if (rejectConfig m c) then false
    else loop (step m c)  in

  loop (startConfig m w)


let rec pairs xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []

(* QUESTION 1 *)
let rec triples_helper l =
  match l with
  | [] -> []
  | hd :: tl ->
    match hd with 
    | (a, (b, c)) -> (a, b, c) :: (triples_helper tl)

let triples xs ys zs =
  triples_helper(pairs xs (pairs ys zs))

let rec quads_helper l =
  match l with
  | [] -> []
  | hd :: tl ->
    match hd with 
    | (a, (b, c, d)) -> (a, b, c, d) :: (quads_helper tl)

let quads xs ys zs ws =
  quads_helper(pairs xs (triples ys zs ws))

let rec range n = 
  if n = 0 then
    [n]
  else
    n::range(n-1)



(* QUESTION 2 *)


let transformStates states f =
  List.fold_right (fun x acc -> (f x)::acc) states []


let rec find_original states f target =
  match states with
  | [] -> failwith "cannot find original value"
  | hd :: tl ->
      if (f hd) = target then
        hd
      else
        (find_original tl f target)

  
let transformDelta states delta f =
  fun (a, b) ->
    let deltaOutput = delta ((find_original states f a), b) in
      match deltaOutput with
      | (x, y, z) ->
        ((f x), y, z)
  

let transform m f = 
  let newStates = transformStates m.states f in
    let newDelta = transformDelta m.states m.delta f in
      {states = newStates;
      input_alphabet = m.input_alphabet;
      tape_alphabet = m.tape_alphabet;
      start = f m.start;
      accept = f m.accept;
      reject = f m.reject;
      blank = m.blank;
      left_marker = m.left_marker;
      delta = newDelta}

(* 
 * Some sample deterministic Turing machines with structured states
 *
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * add1  accepts strings u#v where v = u+1 in binary
 *
 *)


let anbn = { states = [ ("start",0); 
			("q",1);
			("q",2);
			("q",3);
			("q",4);
			("acc",0);
			("rej",0) ];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = ("start",0);
	     accept = ("acc",0);
	     reject = ("rej",0);
	     delta = (fun inp -> match inp with
	                 | (("start",0), "a") -> (("start",0), "a", 1)
     			 | (("start",0), "b") -> (("q",1), "b", 1)
			 | (("start",0), "|") -> (("start",0), "|", 1)
			 | (("start",0), "/") -> (("q",2), "/", 1)
			 | (("q",1), "b") -> (("q",1), "b", 1)
			 | (("q",1), "/") -> (("q",2), "/", 1)
			 | (("q",2), "|") -> (("q",3), "|", 1)
			 | (("q",2), "a") -> (("q",2), "a", 0)
			 | (("q",2), "b") -> (("q",2), "b", 0)
			 | (("q",2), "X") -> (("q",2), "X", 0)
			 | (("q",2), "/") -> (("q",2), "/", 0)
			 | (("q",3), "X") -> (("q",3), "X", 1)
			 | (("q",3), "/") -> (("acc",0), "/", 1)
			 | (("q",3), "a") -> (("q",4), "X", 1)
			 | (("q",4), "a") -> (("q",4), "a", 1)
			 | (("q",4), "X") -> (("q",4), "X", 1)
			 | (("q",4), "b") -> (("q",2), "X", 1)
			 | (("acc",0), s) -> (("acc",0),s,1)
			 | (_,c) -> (("rej",0),c,1))}


let add1 = 
  { states =    (* spelled out fully so as not to rely on 'triples' *)
[("start", -1, -1); ("start", -1, 0); ("start", -1, 1); ("start", 0, -1);
 ("start", 0, 0); ("start", 0, 1); ("start", 1, -1); ("start", 1, 0);
 ("start", 1, 1); ("check1", -1, -1); ("check1", -1, 0); ("check1", -1, 1);
 ("check1", 0, -1); ("check1", 0, 0); ("check1", 0, 1); ("check1", 1, -1);
 ("check1", 1, 0); ("check1", 1, 1); ("check2", -1, -1); ("check2", -1, 0);
 ("check2", -1, 1); ("check2", 0, -1); ("check2", 0, 0); ("check2", 0, 1);
 ("check2", 1, -1); ("check2", 1, 0); ("check2", 1, 1); ("rewind", -1, -1);
 ("rewind", -1, 0); ("rewind", -1, 1); ("rewind", 0, -1); ("rewind", 0, 0);
 ("rewind", 0, 1); ("rewind", 1, -1); ("rewind", 1, 0); ("rewind", 1, 1);
 ("go-end-1", -1, -1); ("go-end-1", -1, 0); ("go-end-1", -1, 1);
 ("go-end-1", 0, -1); ("go-end-1", 0, 0); ("go-end-1", 0, 1);
 ("go-end-1", 1, -1); ("go-end-1", 1, 0); ("go-end-1", 1, 1);
 ("go-end-2", -1, -1); ("go-end-2", -1, 0); ("go-end-2", -1, 1);
 ("go-end-2", 0, -1); ("go-end-2", 0, 0); ("go-end-2", 0, 1);
 ("go-end-2", 1, -1); ("go-end-2", 1, 0); ("go-end-2", 1, 1);
 ("skip", -1, -1); ("skip", -1, 0); ("skip", -1, 1); ("skip", 0, -1);
 ("skip", 0, 0); ("skip", 0, 1); ("skip", 1, -1); ("skip", 1, 0);
 ("skip", 1, 1); ("scan-1", -1, -1); ("scan-1", -1, 0); ("scan-1", -1, 1);
 ("scan-1", 0, -1); ("scan-1", 0, 0); ("scan-1", 0, 1); ("scan-1", 1, -1);
 ("scan-1", 1, 0); ("scan-1", 1, 1); ("scan-2", -1, -1); ("scan-2", -1, 0);
 ("scan-2", -1, 1); ("scan-2", 0, -1); ("scan-2", 0, 0); ("scan-2", 0, 1);
 ("scan-2", 1, -1); ("scan-2", 1, 0); ("scan-2", 1, 1);
 ("check-done", -1, -1); ("check-done", -1, 0); ("check-done", -1, 1);
 ("check-done", 0, -1); ("check-done", 0, 0); ("check-done", 0, 1);
 ("check-done", 1, -1); ("check-done", 1, 0); ("check-done", 1, 1)];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";"_";">"];
    blank = "_";
    left_marker = ">";
    start = ("start",-1,-1);
    accept = ("acc",-1,-1);
    reject = ("rej",-1,-1);
    delta = (fun x -> match x with
    | (("start",-1,-1),">") -> (("check1",-1,-1),">",1)
    | (("check1",-1,-1),"0") -> (("check1",-1,-1),"0",1)
    | (("check1",-1,-1),"1") -> (("check1",-1,-1),"1",1)
    | (("check1",-1,-1),"#") -> (("check2",-1,-1),"#",1)
    | (("check2",-1,-1),"0") -> (("check2",-1,-1),"0",1)
    | (("check2",-1,-1),"1") -> (("check2",-1,-1),"1",1)
    | (("check2",-1,-1),"_") -> (("rewind",-1,1),"_",0)   (* start with a carry of 1! *)

    | (("rewind",-1,carry),">") -> (("go-end-1",-1,carry),">",1)
    | (("rewind",-1,carry),"0") -> (("rewind",-1,carry),"0",0)
    | (("rewind",-1,carry),"1") -> (("rewind",-1,carry),"1",0)
    | (("rewind",-1,carry),"#") -> (("rewind",-1,carry),"#",0)
    | (("rewind",-1,carry),"X") -> (("rewind",-1,carry),"X",0)

    | (("go-end-1",-1,carry),"#") -> (("scan-1",-1,carry),"#",0)
    | (("go-end-1",-1,carry),sym) -> (("go-end-1",-1,carry),sym,1)

    | (("scan-1",-1,carry),"X") -> (("scan-1",-1,carry),"X",0)
    | (("scan-1",-1,carry),"0") -> (("skip",0,carry),"X",1)
    | (("scan-1",-1,carry),"1") -> (("skip",1,carry),"X",1)
    | (("scan-1",-1,0),">") -> (("check-done",-1,-1),">",1)  (* carry should be 0 to be done *)

    | (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
    | (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

    | (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
    | (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

    | (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
    | (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",-1,(v+carry) / 2),"X",0)
    | (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",-1,(v+carry) / 2),"X",0)

    | (("check-done",-1,-1),"_") -> (("acc",-1,-1),"_",1)
    | (("check-done",-1,-1),"X") -> (("check-done",-1,-1),"X",1)
    | (("check-done",-1,-1),"#") -> (("check-done",-1,-1),"#",1)

    | (_,sym) -> (("rej",-1,-1),sym,1))}


(* QUESTION 3 *)

(* 1 is right
  0 is left *)
let permutation_symbol = 
  { states = pairs ["start"; "searching"; "forward1"; "forward2"; "rewind1"; "rewind2"; 
                    "rewind3"; "acc_fwd1"; "acc_fwd2"; "acc"; "rej"] 
                  [">"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; 
                  "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"; "X"];

    input_alphabet = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
                      "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"];
    tape_alphabet = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
                      "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z";
                      "#"; "X"; "_"; ">"];
    start = ("start", ">");
    accept = ("acc", "X");
    reject = ("rej", "X");
    blank = "_";
    left_marker = ">";
    delta = (fun x -> match x with
              | (("start", ">"), ">") -> (("searching", ">"), ">", 1)
              | (("searching", letter), "_") -> (("rej", "X"), "_", 1)
              | (("searching", letter), sym) -> (("forward1", sym), "X", 1)
              | (("forward1", letter), "_") -> (("rej", "X"), "_", 1)
              | (("forward1", letter), "#") -> (("forward2", letter), "#", 1)
              | (("forward1", letter), sym) -> (("forward1", letter), sym, 1)
              | (("forward2", letter), "_") -> (("rej", "X"), "_", 1)
              | (("forward2", letter), sym) when sym = letter -> (("rewind1", "X"), "X", 0)
              | (("forward2", letter), sym) -> (("forward2", letter), sym, 1)

              | (("rewind1", letter), "#") -> (("rewind2", "X"), "#", 0)
              | (("rewind1", letter), sym) -> (("rewind1", "X"), sym, 0)
              
              | (("rewind2", letter), "X") -> (("acc_fwd1", "X"), "X", 1)
              | (("rewind2", letter), sym) -> (("rewind3", "X"), sym, 0)
              
              | (("rewind3", letter), "X") -> (("searching", "X"), "X", 1)
              | (("rewind3", letter), sym) -> (("rewind3", "X"), sym, 0)

              | (("acc_fwd1", "X"), "#") -> (("acc_fwd2", "X"), "#", 1)
              | (("acc_fwd2", "X"), "X") -> (("acc_fwd2", "X"), "X", 1)
              | (("acc_fwd2", "X"), "_") -> (("acc", "X"), "_", 1)

              | (_,sym) -> (("rej", "X"),sym,1))
      }

let permutation = transform permutation_symbol (fun (x, y) -> x^"|"^y)

(* tuple order is (state, copy_num, last_read_bit) *)
(* 1 is right
  0 is left *)
let copies in_n = 
  if in_n < 1 then
    failwith "n must be greater than 0"

  else
  let rangeN = range in_n in
  transform
  { states = triples ["start"; "searching"; "forward"; "check"; "check_last"; "rewind"; "verify"; "acc"; "rej"] rangeN [">"; "X"; "0"; "1"; "X"; "_"];
    input_alphabet = ["0"; "1"];
    tape_alphabet = ["0"; "1"; ">"; "X"; "#"; "_"];
    start = ("start", 1, ">");
    accept = ("acc", -1, "X");
    reject = ("rej", -1, "X");
    blank = "_";
    left_marker = ">";
    delta = (fun x -> match x with
            | (("start", 1, ">"), ">") -> (("searching", 1, ">"), ">", 1)
            
            | (("searching", n, bit), "#") -> (("verify", 1, bit), "#", 1)
            | (("searching", n, bit), "X") -> (("searching", n, bit), "X", 1)
            | (("searching", n, bit), sym) -> (("forward", n, sym), "X", 1)

            | (("forward", n, bit), "#") when (n+1) = in_n -> (("check_last", n+1, bit), "#", 1)
            | (("forward", n, bit), "#") -> (("check", n+1, bit), "#", 1)
            | (("forward", n, bit), "_") when in_n = 1 -> (("acc", -1, "X"), "_", 1)
            | (("forward", n, bit), "_") -> (("rej", -1, "X"), "_", 1)
            | (("forward", n, bit), sym) -> (("forward", n, bit), sym, 1)

            | (("check_last", n, bit), "X") -> (("check_last", n, bit), "X", 1)
            | (("check_last", n, bit), sym) when sym = bit -> (("rewind", 1, bit), "X", 0)

            | (("check", n, bit), "X") -> (("check", n, bit), "X", 1)
            | (("check", n, bit), sym) when sym = bit -> (("forward", n, bit), "X", 1)

            | (("rewind", n, bit), ">") -> (("searching", n, bit), ">", 1)
            | (("rewind", n, bit), sym) -> (("rewind", n, bit), sym, 0)

            | (("verify", n, bit), "#") when n > in_n -> (("rej", -1, "X"), "#", 1)
            | (("verify", n, bit), "#") -> (("verify", n+1, bit), "#", 1)
            | (("verify", n, bit), "X") -> (("verify", n, bit), "X", 1)
            | (("verify", n, bit), "_") when n = (in_n - 1) -> (("acc", -1, "X"), "_", 1)
            | (("verify", n, bit), sym) -> (("rej", -1, "X"), sym, 1)

            | (_,sym) -> (("rej", -1, "X"),sym,1))} 

    (fun (x,y,z) -> x^"|"^(string_of_int y)^"|"^z)





