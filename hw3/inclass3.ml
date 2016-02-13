let rec squares (in_list) = 
	match in_list with
	| [] -> []
	| hd :: tl ->
		hd*hd::squares(tl)

let rec diag (in_list) = 
	match in_list with
	| [] -> []
	| hd :: tl ->
		(hd, hd)::diag(tl)

let rec lengths (in_list) = 
	match in_list with
	| [] -> []
	| hd :: tl ->
		List.length(hd)::lengths(tl)

(* NOTE:
See how the above three functions follow a pattern of

let rec NAME xs = 
	match xs with
	|[] -> []
	| x::xs' -> (ZZZ x)::(NAME xs')

We can generalize this with a map
 *)

let rec map (f, xs) = 
	match xs with
	|[] -> []
	| x :: xs' -> f(x)::map(f, xs')

(* NOTE:
We can define a function in place and not give it a name like so:
fun x -> x + 1;;

So we can use it in map
map((fun x -> x + 1), [1;2;3]);;
>> [2;3;4]

We can also name it
let f = fun x -> x + 1;;
 *)

let diags = fun x -> (x, x, x)
let thirds = fun (a, b, c) -> c
let thirds_2 = fun x -> match x with (a,b,c) -> c

(* 
map((fun x->(x,x,x), [1;2;3]);;
>> [(1;1;1), (2;2;2), (3;3;3)]
 *)

let dist (y, xs) = 
	map ((fun x -> (y, x)), xs)

let dist_2 (y, xs) = 
	let helper x = (y x) in
		map (helper, xs)

let create_mkPair y = 
	(fun x -> (y, x))

let dist_3 (y, xs) = 
	map (create_mkPair(y), xs)

(* 
dist ("h", [1;2;3]);
>> [("h1", 1); ("h", 2); ("h", 3)]

Note that create_mkPair is CREATING a function that attaches y to x
We've written a function that creates functions
 *)

let create_mkPair_2 y x = (y, x)
let add x y = x + y
let incr = add 1

(* 
In OCaml, arrows are associative
a -> b -> c
is also
a -> (b -> c) 

create_mkPair_2 is another way to write the same function--an abbreviations
for create_mkPair

add function adds two arguments together
incr is a function creates a function with add, with one argument of add being 1

let add x y = x+y
Functions created like above are called a CURRIED function

add 3;;
>> int -> int = <fun>

(add 1) 3;;
>> int = 4

add 1 3;;
>> int 4

In the last case, since curried functions are associated to the left, it first
calls add 1, get a function, then calls that function with 3 to essentially get 1+3 = 4
*)


let rec removeEmpty (in_list) = 
	match in_list with
	| [] -> []
	| hd :: tl ->
		if List.length(hd) = 0 then
			removeEmpty(tl)
		else
			hd::removeEmpty(tl)

let odd n = (n mod 2 = 1)

let rec filter (predicate, in_list) = 
	match in_list with
	| [] -> []
	| hd :: tl ->
		if predicate hd then
			hd :: filter(predicate, tl)
		else
			filter(predicate, tl)

(* 
Note that map is a one-to-one function. The list it returns is the same size
as the input list. Filter drops elements depending on the predicate. So, the 
original map function that we wrote before isn't general enough to handle filter
 *)

 let rec map f xs = 
	match xs with
	|[] -> []
	| x :: xs' -> (f x)::(map f xs')
(* map is a function of the type: 
('a -> 'b) -> 'a list -> 'b list 
*)

let rec map_append f xs = 
	match xs with 
	| [] -> []
	| x :: xs' -> (f x)@(map_append f xs')
(* map_append has the type
('a -> 'b list) -> 'a list -> 'b list 

map_append (fun x -> [x; x+1; x+2]) [10;20;30];;
>>  int list = [10; 11; 12; 20; 21; 22; 30; 31; 32]
*)

let flatten xss = 
	map_append (fun x -> x) xss

let filter p xs = 
	map_append (fun x -> if (p x) then [x] else [] ) xs
(* p = predicate
comb = combinator function
Note that the shape of flatten and filter are quite similar. We can
generalize them like so: *)

let rec map_general1 comb f xs =
	match xs with
	| [] -> []
	| x :: xs' -> comb (f x) (map_general1 comb f xs')

let map_2 f xs = map_general1 (fun x y -> x::y) f xs
let map_append_2 f xs = map_general1 (fun x y -> x@y) f xs
(* Note how map_2 and map_append_2 are the same as the original version,
we just pass a function we define in as a combinator function *)

let rec map_general2 comb xs = 
	match xs with
	| [] -> []
	| x :: xs' -> comb x (map_general2 comb xs')

let map_3 f xs = map_general2 (fun x y -> (f x)::y) xs
let map_append_3 f xs = map_general2 (fun x y -> (f x)@y) xs
(* In map_general2, we combine the function transform and the combinator
function together to make it a bit more elegant *)

let rec sum xs =
	match xs with
	| [] -> 0
	| x :: xs' -> x + sum xs'

let rec sum_general comb xs = 
	match xs with
	| [] -> 0
	| x :: xs' -> comb x (sum_general comb xs')
(* See how sum_general and map_general2 is identical save for the type
returned in the base case? Let's generalize more *)

let rec general comb xs base =
	match xs with 
	| [] -> base
	| x :: xs' -> comb x (general comb xs' base)
(* 
general (fun x y -> x+y) [1;2;3;4;5] 0;;
>> int = 15

general (fun x y -> x+y) [1;2;3;4;5] 100;;
>> int = 115

general (fun x y -> (x+1)::y) [1;2;3;4;5] [];;
>> int list = [2; 3; 4; 5; 6]

Woooah so cool. Essentially, this general function is the definition of
recursing over a list. We've generalized everything else away. The computer
science name of this function is "fold_right" or "reduce"
*)

let rec fold_right comb xs base =
	match xs with 
	| [] -> base
	| x :: xs' -> comb x (fold_right comb xs' base)





