open Epistemic_logic

(* m_a = 0,   m_b = 1,   d = 2 *)
let f1 = Know(0, Bin(Not(AP(0)), Or, Not(AP(1))))
let f2 = Know(0, Know(1, Not(AP(2))))

let exec () =
  print_string ("f1 = " ^ pp_of_fmla f1 ^ "\n");
  print_string ("f2 = " ^ pp_of_fmla f2 ^ "\n");