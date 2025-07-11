type priority = int

type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

val empty : 'a queue

val is_empty : 'a queue -> bool

val insert : 'a queue -> priority -> 'a -> 'a queue

exception Queue_is_empty

val remove_top : 'a queue -> 'a queue

val extract : 'a queue -> priority * 'a * 'a queue