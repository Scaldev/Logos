open Epistemic_logic

let f1 = Know("a", Bin(Not(AP("p")), Or, Not(AP("q"))))
let f2 = Know("b", Know("a", Not(AP("r"))))
let f3 = Bin (Bin (AP "p", Imp, AP "q"), And, Bin (AP "r", Eq, AP "s"))
let f4 = Not(Bin(True, Or, False))

let exec () =
  print_string ("f1 = " ^ pp_of_fmla f1 ^ "\n");
  print_string ("f2 = " ^ pp_of_fmla f2 ^ "\n");
  print_string ("f3 = " ^ pp_of_fmla f3 ^ "\n");
  print_string ("f4 = " ^ pp_of_fmla f4 ^ "\n");

