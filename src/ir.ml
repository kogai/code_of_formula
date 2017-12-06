(* filename, line, column *)
type info = string * int * int
[@@deriving show]

type t =
  | Number of info * float
  | Operator of info * string * t * t
  | Identifier of info * string
  [@@deriving show]

let create_info file line column = (file, line, column)
