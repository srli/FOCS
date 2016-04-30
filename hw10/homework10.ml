(*

HOMEWORK 10

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


type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree


let sample = Node(10,Node(3,Node(7,Empty,Empty),
                            Node(5,Empty,Empty)),
                     Node(6,Node(99,Empty,
                                 Node(66,Empty,Empty)),
                          Empty))


(* Printing an integer binary tree *)

let pbt bt =
  let rec loop bt depth =
    match bt with
    | Empty -> ()
    | Node(n,left,right) ->
	(loop right (depth^"    ");
         print_endline (depth^(string_of_int n));
         loop left (depth^"    ")) in
  loop bt ""


(* Q1 *)

let rec size t =
  let rec loop t depth =
  match t with
  | Empty -> 0
  | Node(n, left, right) ->
    (loop right depth)+(loop left depth)+1
  in loop t ""


let rec sum t =
  let rec loop t depth =
  match t with
  | Empty -> 0
  | Node(n, left, right) ->
    (loop right depth)+(loop left depth)+n
  in loop t ""

let rec height t =
  let rec loop t depth =
  match t with
  | Empty -> 0
  | Node(n, left, right) ->
    1 + (max (loop right depth) (loop left depth))
  in loop t ""

let rec fringe t =
  match t with
  | Empty -> []
  | Node(n, left, right) ->
    if (left = Empty) then
      if (right = Empty) then
        [n]
      else
        (fringe left)@fringe(right)
    else
      (fringe left)@fringe(right)

let rec map f t =
  match t with
  | Empty -> Empty
  | Node(n, left, right) ->
      Node((f n), (map f left), (map f right))

let rec fold f t b =
  match t with
  | Empty -> b
  | Node(n, left, right) ->
    f n (fold f left b) (fold f right b)

let preorder t =
  let pre_o = (fun v l r -> v::l@r) in
    fold pre_o t []

let postorder t =
  let pos_o = (fun v l r -> l@r@[v]) in
    fold pos_o t []

let inorder t =
  let ino_o = (fun v l r -> l@[v]@r) in
    fold ino_o t []

let rec bst_insert t x =
  match t with
  | Empty -> Node(x, Empty, Empty)
  | Node(n, left, right) ->
    if n < x then
        Node(n, left, (bst_insert right x))
    else if n > x then
        Node(n, (bst_insert left x), right)
    else
      Node(n, left, right)

let rec bst_lookup t x =
  match t with
  | Empty -> false
  | Node(n, left, right) ->
    if n = x then
      true
    else if n > x then
      bst_lookup left x
    else
      bst_lookup right x

let rec bstify_helper nodes =
  match nodes with
  | [] -> Empty
  | hd :: tl ->
      bst_insert (bstify_helper tl) hd

let bstify t =
  bstify_helper (preorder t)

let rec rotate t dir =
  match t with
  | Empty -> Empty
  | Node(n, left, right) ->
    if dir = "ll" then
      match right with
      |Empty -> Empty
      |Node(r_head, r_left, r_right) ->
        Node(r_head, Node(n, left, r_left), r_right)
    else if dir = "rr" then
      match left with
      |Empty -> Empty
      |Node(l_head, l_left, l_right) ->
        Node(l_head, l_left, Node(n, l_right, right))
    else if dir = "lr" then
      Node(n, left, (rotate right "rr"))
    else if dir = "rl" then
      Node(n, (rotate left "ll"), right)
    else
      Empty

let rec avl_helper t x=
  match t with
  | Empty -> Empty
  | Node(n, left, right) ->
    if (height left) - (height right) > 1 then
      Node(n, (avl_helper (rotate left "ll")), (avl_helper right))
    else if (height left) - (height right) < -1 then
      Node(n, (avl_helper left), (avl_helper (rotate right "rr")))
    else
      Node(n, (avl_helper left), (avl_helper right))

let avl_insert t x =
  avl_helper (bst_insert t x)
