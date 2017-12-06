(* filename, line, column *)
type info = string * int * int
[@@deriving show]

type t =
  | Number of info * float
  | Operator of info * string * t * t
  [@@deriving show]

(* type t =
  | TermVar of info * string
  | TermAbs of info * string * t
  | TermApp of info * t * t
  | TermIf of info * t * t * t
  | TermBool of info * bool
  | TermNat of info * int
[@@deriving show] *)

let show_info (file, line, column) = Core.sprintf "%s:%d:%d" file line column
let create_info file line column = (file, line, column)
