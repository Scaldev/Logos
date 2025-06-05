include Product

let (<<) = Fun.compose

(* The goal of this file is to find a solution to a planning task *)

type planning_task = {
  init: state;
  actions: action list;
  goal: fmla;
}

let size_of_planning_task (t: planning_task) : int =
  size_of_state t.init
  + List.fold_right ((+) << size_of_action) t.actions 0
  + size_of_fmla t.goal

let is_in_actions (t: planning_task) (seq: action list) : bool =
  List.for_all (fun alpha -> List.mem alpha t.actions) seq

(**
  Precondition: each action from feq is in t.actions.
*)
let is_solution (t: planning_task) (seq: action list) : bool =
  if not (is_in_actions t seq) then false else
  let s = ref t.init in
  List.iter (fun alpha ->
    if is_applicable !s alpha then
      s := !s @ alpha;
  ) seq;
  !s |= t.goal

let branches_of_state (actions: action list) (s: state) : (state * action) list =
  actions
  |> List.filter (fun a -> is_applicable s a)
  |> List.map (fun a -> s @ a, a) 

let bfs_solution (t: planning_task) (max_depth: int option) : action list option =

  let root = (t.init, []) in
  let pqueue = ref (Pqueue.insert Pqueue.empty 1 root) in
  let answer = ref None in
  let visited = Hashtbl.create 1000 in
  let d = ref 0 in

  while (Option.is_none !answer) && not (Pqueue.is_empty !pqueue) do

    let (_, (s, hist), pq) = Pqueue.extract !pqueue in
    pqueue := pq;
    let depth = List.length hist in

    if depth > !d then (
      d := depth;
    );

    let nb_worlds = Array.length (fst s).domain in
    print_string ("- nb of worlds = " ^ (string_of_int nb_worlds) ^ "\n");
    flush stdout;

    if s |= t.goal then (
      answer := Some (List.rev hist);

    ) else if Option.is_none max_depth || depth < Option.get max_depth then (
      branches_of_state t.actions s
        (* crucial *)
        |> List.filter (fun (s, _) -> not (Hashtbl.mem visited s))
        |> List.map (fun (s, a) -> s, a :: hist)
        |> List.iter (fun (s, a) -> pqueue := Pqueue.insert !pqueue (Array.length (fst s).domain) (s, a))
    )

  done;
  !answer