include module type of Product

type planning_task = {
    init : state;
    actions : action list;
    goal : fmla;
}

val size_of_planning_task : planning_task -> int

val is_solution : planning_task -> action list -> bool

val bfs_solution : planning_task -> int option -> action list option
