open Epistemic_logic

let (<<) = Fun.compose

let ann (nb_agents: int) (f: fmla) (fp: string) : action =
  let e = { eid = 0; pre = f; post = [] } in
  let rs = Array.init nb_agents (fun _ -> [| [0] |]) in
  let em = { events = [| e |]; relations = rs; } in
  let alpha = { name = "ann(" ^ fp ^ ")"; model = em; aid = 0; } in
  alpha

let big_or (fs: fmla list) : fmla =
  let rec aux (gs: fmla list) : fmla =
    match gs with
    | []      -> False
    | [g]     -> g
    | h :: hs -> Bin(h, Or, aux hs)
  in aux fs

let get_aps (n_max: int) : string array =
  let get_ag n = if n mod 2 == 0 then "a" else "b" in
  Array.init (n_max + 1) (fun n -> string_of_int n ^ "_" ^ get_ag n)

let get_valuation (n_max: int) (n: int) : bool array =
  Array.init (n_max+1) (fun i -> i == n || i == n+1)
  
let know_number (n_max: int) (a: int) : fmla =
  let start = 1 - a in
  let len = if n_max < start then 0 else ((n_max - start) / 2) + 1 in
  big_or (List.init len (fun i -> Know (a, AP (start + 2 * i))))


let make_worlds (n_max: int) : world array =
  Array.init n_max (fun n -> { wid = n; valuation = get_valuation n_max n; history = [] }  )

type relations_maker = int -> int * int -> (int * int) list

let make_relations (ws: world array) (n_max: int) (a: int) : relation =
  Array.init n_max (fun i ->
    let lst = [i] in
    let lst = if i mod 2 == a && i > 0                 then (i - 1) :: lst else lst in
    let lst = if i mod 2 <> a && i+1 < Array.length ws then (i + 1) :: lst else lst in
    lst  
  )

(**********************************************************************)

(* Without losing generality, we suppose here that Alice has an even number and Bob an odd number *)
let agents = ["a" ; "b"]

let n_max = 1000
let a_number = 496 (* even *)
let b_number = 497 (* odd *)

let ws = make_worlds n_max

let km = {
  domain = ws;
  relations = [| make_relations ws n_max 0 ; make_relations ws n_max 1 |]
}

let w = min a_number b_number

let s_0 = (km, w)

let phi_a = know_number n_max 0
let phi_b = know_number n_max 1

let actions = [
  ann 2 phi_a "phi_a";
  ann 2 phi_b "phi_b";
  ann 2 (Not phi_a) "non(phi_a)";
  ann 2 (Not phi_b) "non(phi_b)";
]

let goal = Bin (phi_a, And, phi_b)

let t = {
  init = s_0 ;
  actions = actions ;
  goal = goal ;
}

(***********************)

let my_solution = [ann 2 (Not phi_a) "non(phi_a)" ; ann 2 phi_b "phi_b"]

let solution = bfs_solution t

let max_depth = Some 500

let solve (t: planning_task) : string =
  print_string "Solving...\n";
  match bfs_solution t max_depth with
  | None -> "No solution\n"
  | Some alphas -> "Solution (" ^ (string_of_int (List.length alphas)) ^ "actions): \n" ^ List.fold_left (fun acc -> fun alpha -> acc ^ "- " ^ alpha.name ^ "\n") "" alphas ^ "\n"

let exec () =
  let answer = solve t in
  print_string answer;