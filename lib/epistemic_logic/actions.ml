include Syntax

module StringSet = Set.Make(String)

let (<<) = Fun.compose

(*****************************************************************************)
(*                                   Event                                   *)
(*****************************************************************************)

type 'a relation = ('a * 'a) list

type event = {
  pre: fmla;
  post: (string * fmla) list;
}
  
(*****************************************************************************)

let post (e: event) (p: string) : fmla =
  match List.find_opt ((=) p << fst) e.post with
  | None -> AP p
  | Some (_, f) -> f
    
(*****************************************************************************)

let size_of_event (e: event) : int = 
  size_of_fmla e.pre + List.fold_right ((+) << size_of_fmla << snd) e.post 0
  
(*****************************************************************************)

(**
  [aps_of_events e] returns all atomic propositions mentionned in the precondition
  or one of the postconditions of [e].
  In particular, the returned list contains all atomic propositions such that
  [e.pre.(p) <> p].
*)
let aps_of_event (e: event) : StringSet.t =
  let f = StringSet.union << StringSet.of_list << aps_of_fmla << snd in
  StringSet.union
    (StringSet.of_list (aps_of_fmla e.pre))
    (List.fold_right f e.post (StringSet.empty))

let aps_of_events (es: event list): string list =
  StringSet.to_list (List.fold_right (StringSet.union << aps_of_event) es (StringSet.empty))

(*****************************************************************************)

(**
  [utf8_length s] returns the utf-8 length of a string. This means special characters
  such as the ones used in formules only count as being of size one.
*)
let utf8_length (s: string): int =
  let decoder = Uutf.decoder (`String s) in
  let rec count acc =
    match Uutf.decode decoder with
    | `Uchar _     -> count (acc + 1)
    | `End         -> acc
    | `Malformed _ -> count (acc + 1)
    | `Await       -> assert false
  in
  count 0;;

(**
  [max_length_of_strings ss] return the length of the longest string in [ss].
*)
let max_length_of_strings (ss: string list) : int =
  List.fold_right (max << utf8_length) ss 0

(**
  [add_space_to_string len s] add space characters to [s] until its length is [>= len].
*)
let add_space_to_string (len: int) (s: string) : string =
  let n = utf8_length s in
  if n >= len then s else s ^ (String.make (len - n) ' ')

(**
  [pp_of_postcondition (p, f)] returns a pretty string representation of the
  postcondition, as a "p := f" string.
  Example: "   m_a := ⊥ "
*)
let pp_of_postcondition ((p, f): string * fmla) : string =
  "   " ^ p ^ " := " ^ pp_of_fmla f ^ " "

(**
  [format_line line] returns a "|{line}|\n" string, representing the line of
  an event postcondition.
*)
let format_line (line: string): string =
  "|" ^ line ^ "|\n"

(**
  [lines_content_of_event e] returns a list of lines to put in the event box.
  - The first element is the pretty-print of the precondition.
  - The second element is the pretty-print of the "post:" line.
  - The remaining elements are the postconditions lines.
*)
let lines_content_of_event (e: event) : string list =
  
  let pp_pre = " pre: " ^ pp_of_fmla e.pre ^ " " in
  let pp_post = " post: " in
  let pp_posts = List.map pp_of_postcondition e.post in

  List.append [pp_pre; pp_post] pp_posts

(**
  [table_of_lines len lines] returns a pretty-print of an event of
  lines [lines], with each line being [len] characters wide.
*)
let table_of_lines (len: int) (lines: string list) : string =

  let line = "+" ^ (String.make len '-') ^ "+\n" in
  let line_pre = List.hd lines in
  let line_post = List.hd (List.tl lines) in
  let line_posts = List.fold_right (^) (List.tl (List.tl lines)) "" in

  line ^ line_pre ^ line ^ line_post ^ line_posts ^ line

let pp_of_event (e: event) : string =
  
  let lines_content = lines_content_of_event e in
  let len = max_length_of_strings lines_content in
  let lines = List.map (format_line << add_space_to_string len) lines_content in

  table_of_lines len lines

(*****************************************************************************)
(*                            Event model & action                           *)
(*****************************************************************************)

(* is [(->)_a], with [a] an agent *)
type 'a relations = (string * 'a relation) list

type event_model = {
  events: event list;
  relations: event relations;
}

type action = event_model * event

(*****************************************************************************)

let size_of_event_model (em: event_model) : int =
  List.length em.events
  + List.fold_right ((+) << List.length << snd) em.relations 0
  + List.fold_right ((+) << size_of_event) em.events 0