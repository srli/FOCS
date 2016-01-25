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
   else if (a - b) == 0 then
      a
   else
      failwith "gcd failure"

let is_coprime (a,b) = 
   if gcd (a, b) == 1 then
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
   failwith "not implemented"


let flatten (xss) = 
   failwith "not implemented"


let nth (n,xs) = 
   failwith "not implemented"


let last (xs) = 
   failwith "not implemented"


let separate (xs) = 
   failwith "not implemented"



(* Question 3 *)

let setIn (e,xs) = 
   failwith "not implemented"


let setSub (xs,ys) = 
   failwith "not implemented"


let setEqual (xs,ys) = 
   failwith "not implemented"


let setUnion (xs,ys) = 
   failwith "not implemented"


let setInter (xs,ys) = 
   failwith "not implemented"


let setSize (xs) = 
   failwith "not implemented"

