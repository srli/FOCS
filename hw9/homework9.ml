(*

HOMEWORK 9

Name:

Email:

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



(* The underlying implementation for streams
 *
 * Basically, a stream is a pair of an element and a
 * "promise" to compute the rest of the stream.
 *
 * That "promise" is represented as a function
 *
 * The implementation memoizes that function: once the function is
 * called once, it remembers its result, and subsequent calls to the
 * function directly return the result without executing the body of
 * the function
 *
 * You don't need to know anything about this code -- you will
 * use functions fby, head, and tail described below instead
 *)

module AbsStream :
  sig
      type 'a stream
      val mk : 'a -> (unit -> 'a stream) -> 'a stream
      val unmk1 : 'a stream -> 'a
      val unmk2 : 'a stream -> 'a stream
      val cst : 'a -> 'a stream
      val fby : 'a stream -> (unit -> 'a stream) -> 'a stream
      val map : ('a -> 'b) -> 'a stream -> 'b stream
      val filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream
      val split : 'a stream -> ('a stream * 'a stream)
      val zip : 'a stream -> 'b stream -> ('a * 'b) stream
      val prefix : int -> 'a stream -> 'a list
      val nth : int -> 'a stream -> 'a
    end =
  struct

    type 'a stream = R of 'a * (unit -> 'a stream)

    let memoize f =
      let memoized = ref None in
      let new_f () =
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f

    let mk h t = R (h, memoize t)
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()

    let rec cst v = mk v (fun () -> cst v)
    let fby s1 ps2 = mk (unmk1 s1) ps2
    let rec map f s = mk (f (unmk1 s)) (fun () -> map f (unmk2 s))
    let rec filter p ctl s = if p (unmk1 ctl) (unmk1 s) then mk (unmk1 s) (fun () -> filter p (unmk2 ctl) (unmk2 s)) else filter p (unmk2 ctl) (unmk2 s)
    let split s = (cst (unmk1 s), unmk2 s)
    let rec zip s1 s2 = mk (unmk1 s1, unmk1 s2) (fun () -> zip (unmk2 s1) (unmk2 s2))

    let rec prefix n s = if n > 0 then (unmk1 s)::(prefix (n-1) (unmk2 s)) else []
    let rec nth n s = if n > 0 then nth (n-1) (unmk2 s) else unmk1 s

  end


(*
 * These are the stream functions you will use
 *
 *)

type 'a stream = 'a AbsStream.stream

let cst : 'a -> 'a stream = AbsStream.cst
        (* constant *)

let fby : 'a stream -> (unit -> 'a stream) -> 'a stream = AbsStream.fby
        (* followed by *)

let map : ('a -> 'b) -> 'a stream -> 'b stream = AbsStream.map
        (* map a function over a stream *)

let filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream = AbsStream.filter
           (* filter a stream based on a control stream and a predicate *)

let zip : 'a stream -> 'b stream -> ('a * 'b) stream = AbsStream.zip
        (* zip two streams into a stream of pairs *)

let split : 'a stream -> ('a stream * 'a stream) = AbsStream.split
          (* split a stream into two streams *)

let prefix : int -> 'a stream -> 'a list = AbsStream.prefix
           (* return the first n elements of a stream *)

let nth : int -> 'a stream -> 'a = AbsStream.nth
           (* return the nth element of a stream *)



(* some useful sample streams, from class *)

let nats =
  let rec natsF () = fby (cst 0)
                         (fun () -> (map (fun x -> x+1) (natsF ()))) in
  natsF ()

let evens = map (fun x -> 2*x) nats
let odds = map (fun x -> x+1) evens



(* this one is cute *)

let ampl =
  let transf (v,(d,m)) =
    if d = 1 && v = m then (v-1,(-1,m))
    else if d = -1 && v = -m then (v+1,(1,m+1))
    else if d = 1 then (v+1,(1,m))
    else (v-1,(-1,m))  in
  let rec f () = fby (zip (cst 0) (cst (1,1)))
                     (fun () -> map transf (f ())) in
  map (fun (x,y) -> x) (f ())


(* streams of the form a0,a1,a2,a3... that are useful for
   illustrating question 3 *)

let tag tg =
  map (fun (t,n) -> t^(string_of_int n)) (zip (cst tg) nats)

let s_a = tag "a"
let s_b = tag "b"



(*
 * QUESTION 1
 *
 *)

let scale n s =
  map (fun x -> n*x) s

let mult s1 s2 =
  map (fun (x, y) -> x * y) (zip s1 s2)

let unzip s =
  ((map (fun (x,y) -> x) s), (map (fun (x,y) -> y) s))

let rec fold f init_s s =
  map (fun (x, y) -> f x y) (zip (fby (init_s) (fun () -> (fold f init_s s))) s)

let drop s =
  (fun (x, y) -> y) (split s)

let first s =
  (fun (x, y) -> x) (split s)

let running_max s =
  fold (fun x y -> if x > y then x else y) (first s) s

(* let rec stutter s = *)
  (* zip s (cst 2) *)





(*
 * QUESTION 2
 *
 *)

(* let plus_neg = map (fun x -> if x mod 2 = 0 then 1 else -1) nats *)
(* let odd_sw = mult plus_neg odds *)
(* let next_at = (fun x y -> y +. (z**x/.x)) *)

let float_in s = map (fun x -> float_of_int x) s
let float_cst z = map (fun x -> x*.z) (float_in (cst 1))
let gn x = (-1.0**((1.0+.x)/.2.0));;

let arctan z =
  map (fun x -> x-.z) (fold (fun x y -> x +. (gn (y+.2.0))*.((z**y)/.y)) (float_cst z) (float_in odds))

let scalef n s =
  map (fun x -> n*.x) s

let pi = map (fun (x, y) -> x-.y) (zip (scalef 16.0 (arctan (1.0/.5.0))) (scalef 4.0 (arctan (1.0/.239.0))))

let rec newton f df guess =
  fold (fun x y -> x -. (f x)/.(df x)) (float_cst guess) nats

let derivative f x =
  map (fun num -> ((f (x +. (1.0/.num)) ) -. (f x)) /.(1.0/.num)) (float_in (drop nats))

let limit epsilon s =
  filter (fun x y -> (x -. y) < epsilon) s (drop s)


(*
 * QUESTION 3
 *
 *)


let rev_prefixes s = failwith "not implemented"

let prefixes s = failwith "not implemented"

let stripes s1 s2 = failwith "not implemented"

let rec flatten ss = failwith "not implemented"

let pairs s1 s2 =  failwith "not implemented"
