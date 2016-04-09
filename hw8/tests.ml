simplify q1_defs "iszero minus _0 _0";;
simplify q1_defs "geq _2 _2";;

# simplify q1_defs "eq _0 _0";;
# simplify q1_defs "eq _1 _2";;
# simplify q1_defs "eq _2 _2";;
# simplify q1_defs "eq _3 _3";;

simplify q1_defs "fst (update_fst (pair a b) c)";;
simplify q1_defs "update_fst (pair a b) c";;

simplify q1_defs "pair a b";;

simplify q1_defs "fst (pair true _1)";;
simplify q2_defs "int _0";;
(if (and (fst m) (fst n)) true
  (if (and (not (fst m)) (not (fst n))) true
    (if (geq (snd m) (snd n) true false))))
