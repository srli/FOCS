(*

HOMEWORK 8

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
 * The implementation of parsing and simplication of lambda terms
 *
 * I've isolated it away into a module that we don't need to peer into
 *
 *)

module LambdaTerms = struct

  type lterm =
      LIdent of string
    | LLam of string * lterm
    | LApp of lterm * lterm

  let fresh =
    let tag = ref 0 in
    fun id ->
      let new_id = id^"_"^(string_of_int (!tag)) in
      ( tag := (!tag) + 1; new_id)

  let lexer = Genlex.make_lexer ["(";")";".";"/"]

  let lex s =
    let str = lexer (Stream.of_string s)  in
    let rec loop () =
      match (Stream.peek str) with
      | None -> []
      | Some _ -> let elt = Stream.next str in elt::(loop())  in
    loop ()

  let expect elt cs =
    match cs with
    | f::cs when f = elt -> Some cs
    | _ -> None

  let expect_ident cs =
    match cs with
    | (Genlex.Ident id)::cs -> Some (id,cs)
    | _ -> None

  let rec parse_term cs =
    match parse_ident_terms cs with
    | Some x -> Some x
    | None ->
	(match parse_lambda cs with
	|	Some x -> Some x
	|	None ->
	    (match parse_group_terms cs with
	    | Some x -> Some x
	    | None ->
		(match parse_ident cs with
		|	Some x -> Some x
		|	None ->
		    (match parse_group cs with
		    | Some x -> Some x
		    | None -> None))))

  and parse_ident_term cs =
    match parse_ident cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_term cs with
	|	None -> None
	|	Some (term2,cs) -> Some (LApp(term1,term2),cs))

  and parse_ident_terms cs =    (* ident term+ *)
    match parse_ident cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_terms cs with
	|	None -> None
	|	Some (f,cs) -> Some (f term1,cs))

  and parse_group_terms cs =    (* group term+ *)
    match parse_group cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_terms cs with
	|	None -> None
	|	Some (f,cs) -> Some (f term1, cs))

  and parse_terms cs =
    match parse_ident cs with
    | Some (term1,cs) ->
	(match parse_terms cs with
	|	None -> Some ((fun t -> LApp(t,term1)),cs)
	|	Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
    | None->
	(match parse_group cs with
	|	Some (term1,cs) ->
	    (match parse_terms cs with
	    | None -> Some ((fun t -> LApp(t,term1)),cs)
	    | Some (f,cs) -> Some ((fun t -> f (LApp (t,term1))),cs))
	|	None -> None)


  and parse_ident cs =
    match expect_ident cs with
    | None -> None
    | Some (id,cs) -> Some (LIdent id,cs)

  and parse_lambda cs =
    match expect (Genlex.Kwd "/") cs with
    | None -> None
    | Some cs ->
	(match expect_ident cs with
	|	None -> None
	|	Some (id,cs) ->
	    (match expect (Genlex.Kwd ".") cs with
	    | None -> None
	    | Some cs ->
		(match parse_term cs with
		|	None -> None
		|	Some (term,cs) -> Some (LLam (id,term),cs))))

  and parse_group_term cs =
    match parse_group cs with
    | None -> None
    | Some (term1,cs) ->
	(match parse_term cs with
	|	None -> None
	|	Some (term2,cs) -> Some (LApp (term1,term2),cs))

  and parse_group cs =
    match expect (Genlex.Kwd "(") cs with
    | None -> None
    | Some cs ->
	(match parse_term cs with
	|	None -> None
	|	Some (term,cs) ->
	    (match expect (Genlex.Kwd ")") cs with
	    | None -> None
	    | Some cs -> Some (term,cs)))

  let parse str =
    match parse_term (lex str) with
    | Some (term,[]) -> term
    | _ -> failwith ("Cannot parse "^str)

  let rec pp term =
    match term with
    | LIdent x -> x
    | LLam (x,t) -> "/"^x^"."^(pp t)
    | LApp (t1,t2) ->
	let t1' = (match t1 with
	| LLam _ -> "("^(pp t1)^")"
	| _ -> pp t1)  in
	let t2' = (match t2 with
	| LApp _ -> "("^(pp t2)^")"
	| LLam _ -> "("^(pp t2)^")"
	| _ -> pp t2)  in
	t1'^" "^t2'


  let rec rename term old nw =
    match term with
    | LIdent x when x = old -> LIdent nw
    | LIdent x -> LIdent x
    | LLam (x,t) when x = old  -> LLam (x,t)
    | LLam (x,t) -> LLam (x, rename t old nw)
    | LApp (t1,t2) -> LApp (rename t1 old nw,
			    rename t2 old nw)

  let rec fv m =
    match m with
    | LIdent x -> [x]
    | LLam (x,t) -> List.filter (fun y -> x <> y) (fv t)
    | LApp (t1,t2) -> (fv t1) @ (fv t2)

  let rec substitute m s n =
    match m with
    | LIdent x when x = s -> n
    | LIdent x -> LIdent x
    | LLam (x,t) when x = s -> LLam (x,t)
    | LLam (x,t) when List.mem x (fv n) ->
	let x_ = fresh x in
	substitute (LLam (x_,rename t x x_)) s n
    | LLam (x,t) -> LLam (x,substitute t s n)
    | LApp (t1,t2) -> LApp (substitute t1 s n,
			    substitute t2 s n)

  let rec reduce term =
    match term with
    | LIdent s -> None
    | LLam (s,term) ->
	(match reduce term with
	|	None -> None
	|	Some t -> Some (LLam(s,t)))
    | LApp (LLam (s,term1),term2) ->
	Some (substitute term1 s term2)
    | LApp (term1,term2) ->
	(match reduce term1 with
	|	None -> (match reduce term2 with
      	  | None -> None
	  | Some t2 -> Some (LApp (term1,t2)))
	|	Some t1 -> Some (LApp (t1,term2)))

  let expand_all defs =
    let expand defs t = List.fold_left (fun r (n,d) -> substitute r n d) t defs in
    let rec loop done_defs defs =
      match defs with
      | [] -> done_defs
      | (name,df)::dfs -> loop ((name,expand done_defs df)::done_defs) dfs  in
    loop [] defs

  let threshold = 5000

  let simplify' print_term defs term =
    let term = parse term  in
    let expand defs t = List.fold_left (fun r (n,d) -> substitute r n d) t defs in
    let defs = expand_all (List.map (fun (n,d) -> (n,parse d)) defs) in
    let term = expand defs term  in
    let rec loop n term =
      let _ = print_term (" = "^(pp term)) in
      if n > threshold
      then failwith ("failed to find normal form after "^(string_of_int threshold)^" simplifications")
      else
	match (reduce term) with
	| None -> pp term
	|	Some term -> loop (n+1) term  in
    match reduce term with
    | None -> let _ = print_endline "Term already in normal form" in (pp term)
    | Some next -> let _ = print_term ("   "^(pp term))  in loop 0 next

end




(*
 * Simplification of lambda terms
 *
 * One version that simply returns the result
 * One version that prints all intermediate terms
 *
 *)

let simplify_verbose defs term = LambdaTerms.simplify' print_endline defs term

let simplify defs term = LambdaTerms.simplify' (fun t -> ()) defs term


(*
 * The default definitions from class
 *
 *)

let default_defs = [ ("true","/x./y.x");
		     ("false","/x./y.y");
		     ("if","/c./x./y.c x y");
		     ("and","/b./c.b c false");
		     ("or","/b./c.b true c");
		     ("not","/b.b false true");
		     ("_0","/f./x.x");
		     ("_1","/f./x.(f x)");
		     ("_2","/f./x.(f (f x))");
		     ("_3","/f./x.(f (f (f x)))");
		     ("_4","/f./x.(f (f (f (f x))))");
		     ("_5","/f./x.(f (f (f (f (f x )))))");
		     ("succ","/n./f./x.(n f) (f x)");
		     ("plus","/m./n./f./x.(m f) (n f x)");
		     ("times","/m./n./f./x.m (n f) x");
		     ("iszero","/n.n (/x.false) true");
		     ("pred","/n./f./x.n (/g.(/h.h (g f))) (/u.x) (/u.u)");
		     ("Y","/f.(/x.f (x x)) (/x.f (x x))");
		     ("fact","Y (/fact./n.(iszero n) _1 (times n (fact (pred n))))") ]


(*************************************************************
 * Question 1
 *
 *************************************************************)

(*
 * By default, all of these are implemented as identifier "not_implemented"
 *
 * Just replace that with your own definition
 *
 *)

let minus = ("minus","/m./n.n pred m")

let geq = ("geq","/m./n.(or (iszero n) (if (iszero (minus n m)) true false))")

let eq = ("eq","/m./n.(and (geq m n) (geq n m))")

let pair = ("pair","/x./y./f.(f x y)")

let match_pair = ("match_pair","/p./f.(p f)")

let fst = ("fst","/p.(match_pair p (/x./y.x))")

let snd = ("snd","/p.(match_pair p (/x./y.y))")

let update_fst = ("update_fst","/p./v.(pair v (snd p))")

let update_snd = ("update_snd","/p./v.(pair (fst p) v)")


(*
 * Make sure all your definitions are added to this -- this is what I'll be testing
 IMPORTANT: Notice that the order of definitions passed into simplify matters.
 Things that are passed into simplify can only refer to things that appear before it in the list
 *
 *)

let q1_defs = default_defs @ [ minus; geq; eq; pair; match_pair; fst; snd; update_fst; update_snd]



(*************************************************************
 * Question 2
 *
 *************************************************************)

(*
 * By default, all of these are implemented as identifier "not_implemented"
 *
 * Just replace that with your own definition
 *
 *)

let int = ("int","/x.(pair true x)")

(* p is the input integer *)
let neg_int = ("neg_int","/p.(pair (if (fst p) false true) (snd p))")

let plus_int = ("plus_int","/m./n.
(pair
  (if (and (fst m) (fst n))
    true
    (if (and (not (fst m)) (not (fst n)))
      false
      (if (and (fst m) (geq (snd m) (snd n)))
        true false)
      )
    )

  (if (or (and (fst m) (fst n)) (and (not (fst m)) (not (fst m))))
    (plus (snd m) (snd n))
    (minus (if (geq (snd m) (snd n)) (snd m) (snd n)) (if (geq snd(m) snd(n)) (snd n) (snd m))))
)")

let times_int = ("times_int","/m./n.
(pair
  (if (or (and (fst m) (fst n)) (and (not (fst m)) (not (fst m)))) true false)
  (times (snd m) (snd n))
)")


(*
 * Make sure all your definitions are added to this -- this is what I'll be testing
 *
 *)

let q2_defs = q1_defs @ [int; neg_int; plus_int; times_int ]


(*************************************************************
 * Question 3
 *
 *************************************************************)

(*
 * By default, all of these are implemented as identifier "not_implemented"
 *
 * Just replace that with your own definition
 *
 *)

let empty = ("empty","not_implemented")

let cons = ("cons","not_implemented")

let match_list = ("match_list","not_implemented")

let length = ("length","not_implemented")

let sum = ("sum","not_implemented")

let append = ("append","not_implemented")

let map = ("map","not_implemented")

let q3_defs = default_defs @ [empty; cons; match_list; length; sum; append; map]
