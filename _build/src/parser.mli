
(* The type of tokens. *)

type token = 
  | PARENTHR of (Ir.info)
  | PARENTHL of (Ir.info)
  | OPERATOR of (Ir.info * string)
  | NUMBER of (Ir.info * float)
  | IDENTIFIER of (Ir.info * string)
  | EQUAL of (Ir.info)
  | EOF of (Ir.info)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ir.t option)
