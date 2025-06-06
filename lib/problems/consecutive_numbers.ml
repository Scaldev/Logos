open Epistemic_logic

(*****************************************************************************)
(*                                Utils functions                            *)
(*****************************************************************************)

(**
  [big_or fs] returns a big or of the formulas of [f].
*)
let big_or (fs: fmla list) : fmla =
  let rec aux (gs: fmla list) : fmla =
    match gs with
    | []      -> False
    | [g]     -> g
    | h :: hs -> Bin(h, Or, aux hs)
  in aux fs

(**
  [know_number n_max a] returns the formula [f] describing the fact that
  agent [a] knows the number of agent [b].
*)
let know_number (n_max: int) (a: int) : fmla =
  let start = 1 - a in
  let len = if n_max < start then 0 else ((n_max - start) / 2) + 1 in
  big_or (List.init len (fun i -> Know (a, AP (start + 2 * i))))

(**
  [ann nb_agents f fp] returns the announce action for formula [f]
  of name "ann([fp])".
*)
let ann (nb_agents: int) (f: fmla) (fp: string) : action =
  let e = { eid = 0; pre = f; post = [] } in
  let rs = Array.init nb_agents (fun _ -> [| [0] |]) in
  let em = { events = [| e |]; relations = rs; } in
  let alpha = { name = "ann(" ^ fp ^ ")"; model = em; aid = 0; } in
  alpha

(**
  [get_valuation n_max n] returns an array of length [n_max+1]
  of [false] elements, except for indexes [n] and [n+1].
*)
let get_valuation (n_max: int) (n: int) : bool array =
  Array.init (n_max+1) (fun i -> i == n || i == n+1)

(**
  [make_worlds n_max] returns the list of worlds for the problem.
*)
let make_worlds (n_max: int) : world array =
  Array.init n_max (fun n -> { wid = n; valuation = get_valuation n_max n; history = [] }  )

(**
  [make_relations ws n_max a] returns the relation for agent [a] given the worlds [ws]
  and the [n_max] value.
*)
let make_relations (ws: world array) (n_max: int) (a: int) : relation =
  Array.init n_max (fun i ->
    let lst = [i] in
    let lst = if i mod 2 == a && i > 0                 then (i - 1) :: lst else lst in
    let lst = if i mod 2 <> a && i+1 < Array.length ws then (i + 1) :: lst else lst in
    lst  
  )

(**
    [get_aps n_max] returns the [aps] string array for the context.
*)
let get_aps (n_max: int) : string array =
  let get_ag n = if n mod 2 == 0 then "a" else "b" in
  Array.init (n_max + 1) (fun n -> string_of_int n ^ "_" ^ get_ag n)

(*****************************************************************************)
(*                                  instanciate                              *)
(*****************************************************************************)

(**
    [create n_max a b] returns a planning task for the consecutive number problem.
*)
let instanciate (n_max: int) (a_number: int) (b_number: int) : planning_task * context =

  if n_max < 1 || a_number < 0 || a_number > n_max || b_number < 0 || b_number > n_max then
    raise (Invalid_argument "a and b should be between 0 and N, with 0 < N");

  if a_number mod 2 == 1 || (a_number + 1 <> b_number && a_number - 1 <> b_number) then
    raise (Invalid_argument "a and b should be consecutive, and a must be even");

  (* Initial state *)
  let ws = make_worlds n_max in
  
  let km = {
    domain = ws;
    relations = [| make_relations ws n_max 0 ; make_relations ws n_max 1 |]
  } in

  let w = min a_number b_number in
  
  (* Actions *)
  let phi_a = know_number n_max 0 in
  let phi_b = know_number n_max 1 in
  
  let actions = [
    ann 2 phi_a "phi_a";
    ann 2 phi_b "phi_b";
    ann 2 (Not phi_a) "non(phi_a)";
    ann 2 (Not phi_b) "non(phi_b)";
  ] in

  let t = {
    init = (km, w) ;
    actions = actions ;
    goal = Bin (phi_a, And, phi_b) ;
  } in

  let ctx = {
    aps = get_aps n_max ;
    ags = [| "a" ; "b" |]
  } in

  (t, ctx)