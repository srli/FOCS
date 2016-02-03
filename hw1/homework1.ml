(* 

HOMEWORK 1

Name: Sophie

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



(* Question 1 *)
let min (a, b) = 
   if a > b then
      b
   else
      a

let rec gcd (a,b) = 
   if abs(a - b) > 0 then
      gcd(abs(a - b), min(a, b))
   else if (a - b) = 0 then
      a
   else
      failwith "gcd failure"

let is_coprime (a,b) = 
   if gcd (a, b) = 1 then
      true
   else
      false

let rec euler_helper(n, i, x) = 
   if i <= n then
      if is_coprime(n, i) then
         euler_helper(n, i+1, x+1)
      else
         euler_helper(n, i+1, x)
   else
      x

let euler (n) = 
   euler_helper(n, 1, 0)

let rec coprimes_helper(n, i, x) = 
   if i <= n then
      if is_coprime(n, i) then
         coprimes_helper(n, i+1, x@[i])
      else
         coprimes_helper(n, i+1, x)
   else
      x

let coprimes (n) = 
   coprimes_helper(n, 1, [])


(* Question 2 *)

let append (xs,ys) = 
   xs @ ys

let rec sum (l) =
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum(tl)

let rec flatten (xss) = 
   match xss with
   | [] -> []
   | hd :: tl -> hd @ flatten(tl)

let rec last (xs) = 
   match xs with
   | [] -> failwith "empty list"
   | [x] -> x
   | hd :: tl -> last(tl)

let rec nth (xs, n) = 
   match n with
   | [] -> failwith "out of bounds"
   | hd :: tl -> 
      if xs > 0 then
         nth(xs - 1, tl)
      else
         hd

let rec separate (lst) =
   match last with 
   | [] -> ([], [])
   | (a, b) :: t -> 
      (match seperate(t) with 
         | (l1, l2) -> (a::l1, b::l2)
      )

(* let rec separate_fst_helper(xs) = 
   match xs with
   | [] -> []
   | (x, y) :: tl -> x::separate_fst_helper(tl)

let rec separate_lst_helper(xs) = 
   match xs with
   | [] -> []
   | (x, y) :: tl -> y::separate_lst_helper(tl)

let rec separate (xs) = 
   (separate_fst_helper(xs), separate_lst_helper(xs))
 *)

(* Question 3 *)

let rec setIn (e,xs) = 
   match xs with
   | [] -> false
   | hd :: tl ->
      if hd = e then
         true
      else
         setIn(e, tl)


let rec setSub_helper(x, ys) =
   match ys with
   | [] -> false
   | hd :: tl ->
      if hd = x then
         true
      else
         setSub_helper(x, tl) 

let rec setSub (xs,ys) = 
   match xs with
   | [] -> true
   | hd :: tl ->
      if setSub_helper(hd, ys) then
         setSub(tl, ys)
      else
         false

let setEqual (xs,ys) = 
   if setSub(xs, ys) then
      if setSub(ys, xs) then
         true
      else
         false
   else
      false

let rec setUnion_helper (zs) = 
   match zs with
   | [] -> []
   | hd :: tl ->
      if setIn(hd, tl) = false then
         hd::setUnion_helper(tl)
      else
         setUnion_helper(tl)

let setUnion (xs,ys) = 
   setUnion_helper(xs@ys)

let rec setInter (xs,ys) = 
   match xs with
   | [] -> []
   | hd :: tl ->
      if setIn(hd, ys) then
         hd::setInter(tl, ys)
      else
         setInter(tl, ys)

let rec setSize_helper (xs) = 
   match xs with
   | [] -> 0
   | hd :: tl -> setSize_helper(tl) + 1

let setSize (xs) = 
   setSize_helper(setUnion_helper(xs))
