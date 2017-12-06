(* filename, line, column *)
type info = string * int * int
[@@deriving show]

type t =
  | Number of info * float
  | Operator of info * string * t * t
  | Identifier of info * string
  | Expression of info * string list * t
[@@deriving show]

let show_info (file, line, column) = Core.sprintf "%s:%d:%d" file line column
let create_info file line column = (file, line, column)

let rec translate name = Core.Printf.(function
    | Number (i, n) -> string_of_float n
    | Operator (i, op, left, right) -> sprintf "%s %s %s" (translate name left) op (translate name right) 
    | Identifier (i, n) -> n
    | Expression (i, vars, ex) ->
      sprintf "const %s = (%s) => %s" name (Core.String.concat ~sep:"," vars) (translate name ex)
  )
