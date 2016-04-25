(*  Data Flow Networks in OCaml
Refer to pg 38 of the notes.

constant string generator-> cst k
map -> map f s
filter -> filter p const s
followed by -> fby s1 (fun () -> s2)    (the second element of followed by is always a function)
zip (maps with 2 inputs) -> zip s1 s2
splitter -> split s1    (this returns a pair of strings)
*)

(* This is the same definition for dataflow for generating natural numbers
we etalked about in class. Observe the order of informaiton flow. The (fun () -> ...)
is a bit strange, but its because OCaml requires recursive calls to be a function*)
let rec natsF k = fby (cst k) (fun () -> map (fun s -> s+1) (natsF k))

let nats = natsf 0
p nats
(*
p nats returns
> int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]*)

(* Generate even numbers *)
let rec evensF k = fby (cst 0) (fn () -> map (fun s -> s+2) (evensF ()))

(*  To add two numbers *)
let plus s1 s2 = map (fun (x, y) -> x + y) (zip s1 s2)

(*  Get triangular numbers*)
let rec triangularF () = plus nats (fby (cst 0) (fun () -> (triangularF ())))

(* Partial sums *)
let drop s =
  let (f, r) = split s in r

let rec psums s = fby s (fun () -> plus (drop s) (psums s))

(* Fibonacci *)
let rec fibF () = fby (cst 0) (fun () -> fby (cst 1) (fun () -> plus (fibF ()) (drop (fibF ()))))
let fib = fibF ()

(* Sieve
<> means not equal*)
let nondivides c elem = elem mod c <> 0
let rec sieve s =
  let (f, r) = split s in
    fby f (fun () -> sieve (filter nondivides f r))

let nats_from_2 = natsF 2
let primes = sieve nats_from_2
(*p primes will then return the first 10 prime numbers*)
