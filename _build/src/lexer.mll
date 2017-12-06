{
open Core
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let { lex_curr_p; lex_curr_pos; } = lexbuf in
  lexbuf.lex_curr_p <- {
    lex_curr_p with pos_bol = lex_curr_pos;
                    pos_lnum = lex_curr_p.pos_lnum + 1
  }

  let info { lex_curr_p; lex_start_pos; } =
    let { pos_fname; pos_lnum; pos_cnum; pos_bol; } = lex_curr_p in
    Ir.create_info pos_fname pos_lnum (pos_cnum - pos_bol)

  let identifier lexbuf =
    Lexing.lexeme lexbuf
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9'] ['0'-'9']* 
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let operator = ['+' '-' '*' '/']

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | operator { OPERATOR (info lexbuf, identifier lexbuf) }
  | digit { NUMBER (
    (info lexbuf),
    (float_of_string (identifier lexbuf))
  )}
  | id { IDENTIFIER ((info lexbuf), (identifier lexbuf)) }
  | '(' { PARENTHL (info lexbuf) }
  | ')' { PARENTHR (info lexbuf) }
  | _ { raise (SyntaxError (sprintf "Unexpected character: [%s]" (Lexing.lexeme lexbuf))) }
  | eof { EOF (info lexbuf) }