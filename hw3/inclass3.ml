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
map((fun x -> x + 1), [1;2;3])
>>>[2;3;4]

We can also name it
let f = fun x -> x + 1;;
 *)

let diags = fun x -> (x, x, x)
let thirds = fun (a, b, c) -> c

(* 
map((fun x->(x,x,x), [1;2;3]);;
>>>[(1;1;1), (2;2;2), (3;3;3)]
 *)

let rec removeEmpty (in_list) = 
	match in_list with
	| [] -> []
	| hd :: tl ->
		if List.length(hd) = 0 then
			removeEmpty(tl)
		else
			hd::removeEmpty(tl)