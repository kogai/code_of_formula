(* filename, line, column *)
type info = string * int * int
[@@deriving show]

type t =
  | Number of info * float
  | Operator of info * string * t * t
  | Identifier of info * string
  | Expression of info * string list * t
[@@deriving show]

let create_info file line column = (file, line, column)

let rec translate = Core.Printf.(function
    | Number (i, n) -> 
      string_of_float n
    | Operator (i, op, left, right) -> sprintf "%s %s %s" (translate left) op (translate right) 
    | Identifier (i, name) -> name
    | Expression (i, vars, ex) -> sprintf "const %s = (%s) => %s" "my_var" (Core.String.concat ~sep:"," vars) (translate ex)
  )
