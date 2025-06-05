open Epistemic_logic

let ctx = {
  aps = [| "p" ; "q" ; "r" ; "s" |];
  ags = [| "a" ; "b "|];
}

let f1 = Know(0, Bin(Not(AP(0)), Or, Not(AP(1))))
let f2 = Know(1, Know(0, Not(AP(2))))
let f3 = Bin (Bin (AP 0, Imp, AP 1), And, Bin (AP 2, Eq, AP 2))
let f4 = Not(Bin(True, Or, False))

let exec () =
  print_string ("f1 = " ^ pp_of_fmla ctx f1 ^ "\n");
  print_string ("f2 = " ^ pp_of_fmla ctx f2 ^ "\n");
  print_string ("f3 = " ^ pp_of_fmla ctx f3 ^ "\n");
  print_string ("f4 = " ^ pp_of_fmla ctx f4 ^ "\n");

