include Syntax

let (<<) = Fun.compose

module StringSet = Set.Make(String)

(*************************************************************************)
(*                           Epistemic actions                           *)
(*************************************************************************)

type 'a relation = ('a * 'a) list

type event = {
  pre: fmla;
  post: (string * fmla) list;
  (* the n-th atomic proposition is mapped to the n-th formula of post *)
}  

let post (e: event) (p: string) : fmla =
  match List.find_opt ((=) p << fst) e.post with
  | None -> AP p
  | Some (_, f) -> f
  
(**
  [aps_of_events e] returns all atomic propositions mentionned in the precondition
  or one of the postconditions of [e].
  In particular, the returned list contains all atomic propositions such that
  [e.pre.(p) <> p].
*)
let aps_of_event (e: event) : string list =
  StringSet.union
    (StringSet.of_list (aps_of_fmla e.pre))
    (List.fold_right (StringSet.union << StringSet.of_list << aps_of_fmla << snd) e.post (StringSet.empty))
  |> StringSet.to_list

let aps_of_events (es: event list): string list =
  List.fold_right (StringSet.union << StringSet.of_list << aps_of_event) es (StringSet.empty)
  |> StringSet.to_list
  
let size_of_event (e: event) : int = 
  size_of_fmla e.pre + List.fold_right ((+) << size_of_fmla << snd) e.post 0

(*****************************************************************************)

(**
  [utf8_length s] returns the utf-8 length of a string. This means special characters
  such as the ones used in formules only count as being of size one.
*)
let utf8_length (s: string): int =
  let decoder = Uutf.decoder (`String s) in
  let rec count acc =
    match Uutf.decode decoder with
    | `Uchar _ -> count (acc + 1)
    | `End -> acc
    | `Malformed _ -> count (acc + 1)
    | `Await -> assert false
  in
  count 0;;

let max_length_of_strings (ss: string list) : int =
  List.fold_right (max << utf8_length) ss 0

let add_space_to_string (n: int) (s: string) : string =
  let len = utf8_length s in
  if len >= n then s else s ^ (String.make (n - len) ' ')

let pp_of_postcondition ((p, f): string * fmla) : string =
  "   " ^ p ^ " := " ^ pp_of_fmla f ^ " "

let format_line (line: string): string =
  "|" ^ line ^ "|\n"

let lines_content_of_event (e: event) : string list =
  
  let pp_pre = " pre: " ^ pp_of_fmla e.pre ^ " " in
  let pp_post = " post: " in
  let pp_posts = List.map pp_of_postcondition e.post in

  List.append [pp_pre; pp_post] pp_posts

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

type 'a arrows = (string * 'a relation) list (* is [(->)_a], with [a] an agent *)

type event_model = {
  events: event list; (* E *)
  rels: event arrows;
  (* the n-th agent relations are at the n-th value of rels *)
}

type action = event_model * event (* id of the actual event *)

(**
  [size em] returns the size of the event model [em].
*)
let size_of_event_model (em: event_model) : int =
  List.length em.events
  + List.fold_right ((+) << List.length << snd) em.rels 0
  + List.fold_right ((+) << size_of_event) em.events 0