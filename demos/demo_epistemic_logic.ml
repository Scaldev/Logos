open Epistemic_logic

(* m_a = 0,   m_b = 1,   d = 2 *)
let f1 = Know(0, Bin(Not(AP(0)), Or, Not(AP(1))))
let f2 = Know(0, Know(1, Not(AP(2))))

let aps = [| "p"; "q"; "r"; "s"; "t" |]
let ags = [| "a"; "b"; "c"; "d" |]

let exec () =
  print_string ("f1 = " ^ pp_of_fmla aps ags f1 ^ "\n");
  print_string ("f2 = " ^ pp_of_fmla aps ags f2 ^ "\n");