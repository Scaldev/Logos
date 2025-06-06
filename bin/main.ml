open Epistemic_logic

let () = print_endline "Hello, World!";;

let problem (t: planning_task) (max_depth: int option) (name: string) : unit =

  print_string ((String.make 80 '#') ^ "\n");
  print_string (name ^ "\n");
  print_string ((String.make 80 '-') ^ "\n");
  let answer = solve max_depth t in
  print_string ((String.make 80 '-') ^ "\n");
  print_string (pp_solution answer);
  print_string ((String.make 80 '-') ^ "\n")


let () =
  let (t_gen, _) = Problems.Consecutive_numbers.instanciate 250 220 219 in
  problem t_gen (Some 500) "CONSECUTIVE NUMBERS" ;
  let (t_csq, _) = Problems.Two_generals.instanciate 20 in
  problem t_csq (Some 500) "TWO GENERALS"