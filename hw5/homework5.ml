(* 

HOMEWORK 5

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
 * Helper function
 *
 * Pint a configuration (including newline) to standard output
 * and RETURN A VALUE
 * 
 *)

let printConfig m config value = 
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	                print_syms v') in
    let _ = print_newline ()  in
    value


(* QUESTION 1 *)
let startConfig m w = 
	{state = m.start;
	before = [];
	after = m.left_marker :: explode w;}

let acceptConfig m config =
	if m.accept = config.state then
		true
	else
		false


let rejectConfig m config =
	if m.reject = config.state then
		true
	else
		false

(* TODO: FIGURE OUT THIS FUNCTION *)
let haltConfig m c =
	if m.accept = c.state then
		true
	else if m.reject = c.state then
		true
	else
		false

let remove_last lst =
	List.rev (List.tl (List.rev lst))

let get_last lst =
	List.hd (List.rev lst)

let get_first m lst =
	match lst with
	| [] -> m.blank
	| hd :: tl ->
		hd

let get_tail lst =
	match lst with
	| [] -> []
	| hd :: tl ->
		tl

let step m config =
	match m.delta (config.state, (get_first m config.after)) with
	| (next_state, to_write, direction) ->
		if direction = 1 then
			{state = next_state;
			 before = config.before@[to_write];
			 after = (get_tail config.after);}
		else
			{state = next_state;
			 before = (remove_last config.before);
			 after = [(get_last config.before)]@[to_write]@(get_tail config.after);}

let rec run_helper m config =
	if (haltConfig m config) then
		config
	else
		run_helper m (printConfig m (step m config) (step m config))

let run m w = 
	let res_config = run_helper m (printConfig m (startConfig m w) (startConfig m w)) in
	if (acceptConfig m res_config) then
		true
	else
		false

(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"_";">"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", ">") -> ("start", ">", 1)
			 | ("start", "_") -> ("acc", "_", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", "|") -> ("start", "|", 1)
			 | ("start", "/") -> ("q2", "/", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "/") -> ("q2", "/", 1)
			 | ("q2", "|") -> ("q3", "|", 1)
			 | ("q2", "a") -> ("q2", "a", 0)
			 | ("q2", "b") -> ("q2", "b", 0)
			 | ("q2", "X") -> ("q2", "X", 0)
			 | ("q2", "/") -> ("q2", "/", 0)
			 | ("q3", "X") -> ("q3", "X", 1)
			 | ("q3", "/") -> ("acc", "/", 1)
			 | ("q3", "a") -> ("q4", "X", 1)
			 | ("q4", "a") -> ("q4", "a", 1)
			 | ("q4", "X") -> ("q4", "X", 1)
			 | ("q4", "b") -> ("q2", "X", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", "|") -> ("acc", "|", 1)
			 | ("acc", "X") -> ("acc", "X", 1)
			 | ("acc", "/") -> ("acc", "/", 1)
			 | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alphabet = ["a";"b";"c"];
	       tape_alphabet = ["a";"b";"c";"X";"_";">"];
	       blank = "_";
	       left_marker = ">";
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = (fun inp -> match inp with
	                | ("start", "a") -> ("start", "a", 1)
     			| ("start", "b") -> ("q1", "b", 1)
			| ("start", "c") -> ("q6", "c", 1)
			| ("start", ">") -> ("start", ">", 1)
			| ("start", "_") -> ("q2", "_", 1)
			| ("q1", "b") -> ("q1", "b", 1)
			| ("q1", "c") -> ("q6", "c", 1)
			| ("q1", "_") -> ("q2", "_", 1)
			| ("q2", ">") -> ("q3", ">", 1)
			| ("q2", "a") -> ("q2", "a", 0)
			| ("q2", "b") -> ("q2", "b", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "_") -> ("q2", "_", 0)
			| ("q2", "X") -> ("q2", "X", 0)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("acc", "_", 1)
			| ("q3", "a") -> ("q4", "X", 1)
			| ("q4", "a") -> ("q4", "a", 1)
			| ("q4", "X") -> ("q4", "X", 1)
			| ("q4", "b") -> ("q5", "X", 1)
			| ("q5", "b") -> ("q5", "b", 1)
			| ("q5", "X") -> ("q5", "X", 1)
			| ("q5", "c") -> ("q2", "X", 1)
			| ("q6", "c") -> ("q6", "c", 1)
			| ("q6", "_") -> ("q2", "_", 1)
		        | ("acc", "a") -> ("acc", "a", 1)
		        | ("acc", "b") -> ("acc", "b", 1)
		        | ("acc", "c") -> ("acc", "c", 1)
		        | ("acc", ">") -> ("acc", ">", 1)
		        | ("acc", "X") -> ("acc", "X", 1)
		        | ("acc", "_") -> ("acc", "_", 1)
			| (_,c) -> ("rej", c,1))}



(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
(* REPLACE BY YOUR OWN DEFINITIONS *)
(* 1 is right
	0 is left *)

let tm_q2_a = { states = ["q1"; "q2"; "q3"; "q4"; "q5"; "q6"; "q7"; "q8"; "acc"; "rej"];
		input_alphabet = ["c"; "d"];
		tape_alphabet = ["c"; "d"; "_"; ">"];
		blank = "_";
		left_marker = ">";
		start = "q1";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with
			 | ("q1", "c") -> ("q2", "x", 1)
			 | ("q1", "d") -> ("q7", "x", 1)
			 | ("q1", "x") -> ("acc", "x", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("q1", ">") -> ("q1", ">", 1)

			 | ("q2", "c") -> ("q3", "c", 1)
			 | ("q2", "d") -> ("q3", "d", 1)
			 | ("q2", "x") -> ("acc", "x", 1)

			 | ("q3", "c") -> ("q3", "c", 1)
			 | ("q3", "d") -> ("q3", "d", 1)
			 | ("q3", "x") -> ("q3", "x", 1)
			 | ("q3", "_") -> ("q4", "_", 0)

			 | ("q4", "c") -> ("q8", "x", 0)
			 | ("q4", "d") -> ("rej", "d", 1)
			 | ("q4", "x") -> ("q4", "x", 0)

			 | ("q7", "c") -> ("q6", "c", 1)
			 | ("q7", "d") -> ("q6", "d", 1)
			 | ("q7", "x") -> ("acc", "x", 1)

			 | ("q6", "c") -> ("q6", "c", 1)
			 | ("q6", "d") -> ("q6", "d", 1)
			 | ("q6", "x") -> ("q6", "x", 1)
			 | ("q6", "_") -> ("q5", "_", 0)

			 | ("q5", "c") -> ("rej", "c", 1)
			 | ("q5", "d") -> ("q8", "x", 0)
			 | ("q5", "x") -> ("q5", "x", 0)

			 | ("q8", "c") -> ("q8", "c", 0)
			 | ("q8", "d") -> ("q8", "d", 0)
			 | ("q8", "x") -> ("q1", "x", 1)

			 | ("acc", "c") -> ("acc", "c", 1)
			 | ("acc", "d") -> ("acc", "d", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}


let tm_q2_b = { states = ["x"];
		input_alphabet = ["x"];
		tape_alphabet = ["x"];
		blank = "x";
		left_marker = "x";
		start = "x";
		accept = "x";
		reject = "x";
		delta = (fun (x,y) -> (x,y,0))}




(* QUESTION 3 *)


let binaryAddition = { states = ["x"];
		       input_alphabet = ["x"];
		       tape_alphabet = ["x"];
		       blank = "x";
		       left_marker = "x";
		       start = "x";
		       accept = "x";
		       reject = "x";
		       delta = (fun (x,y) -> (x,y,0))}

