include Product

let (<<) = Fun.compose

(*****************************************************************************)
(*                                Planning task                              *)
(*****************************************************************************)

type planning_task = {
  init: state;
  actions: action list;
  goal: fmla;
}

let size_of_planning_task (t: planning_task) : int =
  size_of_state t.init
  + List.fold_right ((+) << size_of_action) t.actions 0
  + size_of_fmla t.goal

(*****************************************************************************)
(*                                 is_solution                               *)
(*****************************************************************************)

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

(*****************************************************************************)
(*                             Breadth-first search                          *)
(*****************************************************************************)

let branches_of_state (actions: action list) (s: state) : (state * action) list =
  actions
  |> List.filter (fun a -> is_applicable s a)
  |> List.map (fun a -> s @ a, a) 

let solve (max_depth: int option) (t: planning_task) : action list option =

  let root = (t.init, []) in
  let pqueue = ref (Pqueue.insert Pqueue.empty 1 root) in
  let answer = ref None in
  let visited = Hashtbl.create 1024 in
  let d = ref 0 in

  while Option.is_none !answer && not (Pqueue.is_empty !pqueue) do

    (* Dequeue the priority queue *)
    let (_, (s, hist), pq) = Pqueue.extract !pqueue in
    Hashtbl.add visited s ();
    pqueue := pq;

    (* Check the depth *)
    let depth = List.length hist in
    if depth > !d then
      d := depth;

    let nb_worlds = Array.length (fst s).domain in
    print_string (
      "State n°" ^ (string_of_int (Hashtbl.length visited)) ^ ": " ^
      string_of_int nb_worlds ^ " worlds.\n"
    );
    flush stdout;

    (* Solution found *)
    if s |= t.goal then (
      answer := Some (List.rev hist)
    )
    (* No solution and not max depth *)
    else if Option.is_none max_depth || depth < Option.get max_depth then begin
      branches_of_state t.actions s
        |> List.filter (not << Hashtbl.mem visited << fst)
        |> List.map (fun (s', alpha) -> s', alpha :: hist)
        |> List.iter (fun (s', a) ->
          let prio = Array.length (fst s').domain in
          pqueue := Pqueue.insert !pqueue prio (s', a)
        )
    end

  done;
  !answer

(*****************************************************************************)
(*                                  pp_solution                              *)
(*****************************************************************************)

let pp_solution (alphas: action list option) : string =
  match alphas with
  | None -> "No solution"
  | Some alphas ->
    "Solution (" ^ (string_of_int (List.length alphas)) ^ " actions): \n" ^
    List.fold_left (fun acc -> fun alpha -> acc ^ "- " ^ alpha.name ^ "\n") "" alphas ^ ""
