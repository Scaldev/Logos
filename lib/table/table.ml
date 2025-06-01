let (<<) = Fun.compose

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
  [format_line line] returns a "|{line}|\n" string, representing the line of
  an event postcondition.
*)
let format_line (line: string): string =
  "|" ^ line ^ "|\n"

(**
  [table_of_cells len cells] returns a pretty-print of a table of cells with
  width [len].
*)
let table_of_cells (cells: string list list) : string =
  
  let len = max_length_of_strings (List.flatten cells) in
  let break_line = "+" ^ (String.make len '-') ^ "+\n" in

  cells
  |> List.map (List.map (format_line << add_space_to_string len))
  |> List.map (List.fold_left (^) "")
  |> List.fold_left ((^) << Fun.flip (^) break_line) ""
  |> Fun.flip (^) break_line