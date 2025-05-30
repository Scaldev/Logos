include Syntax

let (<<) = Fun.compose

(*************************************************************************)
(*                           Epistemic actions                           *)
(*************************************************************************)

type 'a relation = ('a * 'a) list

type event = {
  pre: fmla;
  post: fmla option array;
  (* the n-th atomic proposition is mapped to the n-th formula of post *)
}  

let post (e: event) (p: int) : fmla =
  match e.post.(p) with 
  | None -> AP p
  | Some f -> f
  
(**
  [max_ap em] returns the maximum int for an atomic proposition.
*)
let max_ap_in_events (events: event list) =
  List.fold_right max (
    List.map (fun e -> max_ap_in_fmla e.pre) events
  ) 0

(**
  [size_of_event max_ap e] returns the size of the event [e] where the
  maximum int for an atomic proposition in [e]'s postconditions is [max_ap].
*)
let size_of_event (max_ap: int) (e: event) : int = 
  let s = ref (size_of_fmla e.pre) in
  for p = 0 to max_ap do
    if e.post.(p) <> None then
      s := !s + size_of_fmla (post e p)
  done; !s

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

let pp_of_postcondition (aps: string array) (ags: string array) (i: int) (p: fmla option) : string =
  match p with
  | None -> ""
  | Some f -> "   " ^ aps.(i) ^ " := " ^ pp_of_fmla aps ags f ^ " "

let format_line (line: string): string =
  "|" ^ line ^ "|\n"

let lines_content_of_event (aps: string array) (ags: string array) (e: event) : string list =
  
  let pp_pre = " pre: " ^ pp_of_fmla aps ags e.pre ^ " " in
  let pp_post = " post: " in
  let pp_posts =
    Array.to_list e.post
    |> List.filter (((<>) None))
    |> List.mapi (pp_of_postcondition aps ags)
  in

  List.append [pp_pre; pp_post] pp_posts

let table_of_lines (len: int) (lines: string list) : string =

  let line = "+" ^ (String.make len '-') ^ "+\n" in
  let line_pre = List.hd lines in
  let line_post = List.hd (List.tl lines) in
  let line_posts = List.fold_right (^) (List.tl (List.tl lines)) "" in

  line ^ line_pre ^ line ^ line_post ^ line_posts ^ line

let pp_of_event (aps: string array) (ags: string array) (e: event) : string =
  
  let lines_content = lines_content_of_event aps ags e in
  let len = max_length_of_strings lines_content in
  let lines = List.map (format_line << add_space_to_string len) lines_content in
  table_of_lines len lines

(*****************************************************************************)

type event_model = {
  events: event list; (* E *)
  rels: event relation array;
  (* the n-th agent relations are at the n-th value of rels *)
}

type action = event_model * event (* id of the actual event *)

(**
  [size em] returns the size of the event model [em].
*)
let size_of_event_model (em: event_model) : int =
  let max_ap =  max_ap_in_events em.events in
  List.length em.events
  + Array.fold_right ((+) << List.length) em.rels 0
  + List.fold_right ((+) << size_of_event max_ap) em.events 0