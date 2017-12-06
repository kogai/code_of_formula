%{
  open Ir
  open Core
%}

%token <Ir.info * string> IDENTIFIER
%token <Ir.info * float> NUMBER
%token <Ir.info> PLUS
%token <Ir.info> STAR

%token <Ir.info> PARENTHL
%token <Ir.info> PARENTHR
%token <Ir.info> EQUAL

%token <Ir.info> EOF

%left PLUS
%left STAR

%start <Ir.t option> program
%%

program:
  | EOF { None }
  | v = formula { Some v }
formula:
  | left = formula op = PLUS right = formula { Ir.Operator (op, "+", left, right) }
  | left = formula op = STAR right = formula { Ir.Operator (op, "*", left, right) }
  | n = NUMBER { Ir.Number (Tuple2.get1 n, Tuple2.get2 n) }
  | id = IDENTIFIER { Ir.Identifier (Tuple2.get1 n, Tuple2.get2 n) }
/* operate: */
  