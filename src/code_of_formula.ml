open Core
open Lexer
open Parser
open Lexing

let fixture = "x, y = x + x * y + y"

let rec parse lexbuf =
  match Parser.program Lexer.read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let () =
  let lexbuf = Lexing.from_string fixture in
  let results = try
      parse lexbuf
    with
    | Lexer.SyntaxError msg as e ->
      Printf.fprintf stderr "%s%!" msg;
      raise @@ e
    | Parser.Error as e ->
      Printf.fprintf stderr "Parse error [%s] @%s\n" (Lexing.lexeme lexbuf) (Ir.show_info (Lexer.info lexbuf));
      raise @@ e
  in

  List.iter ~f:(fun s -> print_endline @@ Ir.show s) results
