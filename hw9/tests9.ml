prefix 10 (let (s1,s2) = unzip(cst(66,99)) in s1);;
prefix 10 (let (s1,s2) = unzip(stutter nats) in s2);;
prefix 10 (let (s1,s2) = stutter nats in s2);;

  (* map (fun (x, y) -> if y = 2 then x) (zip s (cst 2)) *)

  (* fold (fun x y -> if x = y then x else y) (first s) s *)

prefix 10 (filter (fun x y -> (x +. y) < 0.0) (float_in nats) (float_in nats));;

prefix 10 (flatten (map (fun n -> [n]) nats));;

prefix 10 (flatten (map (fun n -> [n;n+1]) nats));;


prefix 10 (map (fun n -> [n;n+1]) nats);;
