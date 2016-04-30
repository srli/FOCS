pbt (rotate Node(1, Node(2, Node(3, Empty, Empty), Empty), Empty) "rr")
let j = Node(1, Node(2, Node(3, Empty, Empty), Empty), Empty);;

let k = Node(1, Empty, Node(2, Empty, Node(3, Empty, Empty)));;
pbt (rotate k "ll");;

let m = Node(1, Empty, Node(2, Node(3, Empty, Empty), Empty));;
pbt (rotate m "lr");;


T.data > x : return Insert(T.left, x);
            if ((height(T.left)- height(T.right)) = 2){
            if (T.left.data > x ) then //outside case
            T = RotatefromLeft (T);
            else //inside case
            T = DoubleRotatefromLeft (T);}
T.data < x : return Insert(T.right, x);
code similar to the left case

(* type for arithmetic expressions *)
type aexp =
  | Number of int
  | Plus of aexp * aexp
  | Times of aexp * aexp
  | Minus of aexp

(*  size (number of nodes) *)
let rec size t =
  match t with
  | Number (i) -> 1
  | Plus (e, f) -> 1 + size(e) + size(f)
  | Times (e, f) -> 1 + size(e) + size(f)
  | Minus (e) -> 1 + size(e)

(*  height *)
let rec height t =
  match t with
  | Number (i) -> 1
  | Plus (e, f) -> 1 + max (height e) (height f)
  | Times (e, f) -> 1 + max (height e) (height f)
  | Minus (e) -> 1 + height(e)

(* all_numbers (get all the numbers that appear in a tree) *)
let rec all_numbers t =
  match t with
  | Number (i) -> [i]
  | Plus (e, f) -> (all_numbers e)@(all_numbers f)
  | Times (e, f) -> (all_numbers e)@(all_numbers f)
  | Minus (e) -> all_numbers (e)

(* evaluate *)
let rec eval t =
  match t with
  | Number (i) -> i
  | Plus (e, f) -> eval(e) + eval(f)
  | Times (e, f) -> eval(e) * eval(f)
  | Minus (e) -> -(eval(e))
