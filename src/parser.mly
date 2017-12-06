%{
  open Ir
  open Core
%}

%token <Ir.info * string> IDENTIFIER
%token <Ir.info * float> NUMBER
%token <Ir.info * string> OPERATOR

%token <Ir.info> PARENTHL
%token <Ir.info> PARENTHR
%token <Ir.info> EQUAL

%token <Ir.info> EOF
%start <Ir.t option> program
%%

program:
  | EOF { None }
  | v = formula { Some v }
formula:
  | left = formula op = OPERATOR right = formula { Ir.Operator (Tuple2.get1 op, Tuple2.get2 op, left, right) }
  | n = NUMBER { Ir.Number (Tuple2.get1 n, Tuple2.get2 n) }
