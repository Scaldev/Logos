include module type of Product

type planning_task = {
    init : state;
    actions : action list;
    goal : fmla;
}

(**
    [size_of_planning_task t] returns the size of the planning task [t].
*)
val size_of_planning_task : planning_task -> int

(**
    [is_solution t alphas] return [true] iff {[
        s_0 @ a_0 @ a_1 ... @ a_{n-1} |= phi
    ]}
    where [s_0] is the initial state of [t], [phi] is the goal formula of [t],
    and [a_i] are the elements of [alphas] and actions of [t.actions].
*)
val is_solution : planning_task -> action list -> bool

(**
    [solve t depth] executes a breadth-first search on the graph of nodes
    state and of edges actions, trying to find a state [s] such that [s |= t.goal].
    [depth] indicates the maximum depth to search for ; if [None], then the search
    goes on until a solution is found.
*)
val solve : int option -> planning_task -> action list option

(**
    [pp_solution] returns a string for printing the solution.
*)
val pp_solution : action list option -> string