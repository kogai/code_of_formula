%{
  open Ir
  open Core
%}

%token <Ir.info * string> IDENTIFIER
%token <Ir.info * float> NUMBER
%token <Ir.info> PLUS
%token <Ir.info> MINUS
%token <Ir.info> STAR
%token <Ir.info> DIVIDE

%token <Ir.info> PARENTHL
%token <Ir.info> PARENTHR
%token <Ir.info> EQUAL
%token <Ir.info> COMMA
%token <Ir.info> SEMICOLON

%token <Ir.info> EOF

%left PLUS, MINUS
%left STAR, DIVIDE

%start <Ir.t option> program
%%

program:
  | EOF { None }
  | v = formula { Some v }
formula:
  | vs = separated_list(COMMA, variable) e = EQUAL ex = expression EOF { Ir.Expression (e, vs, ex) }
expression:
  | left = expression op = PLUS right = expression { Ir.Operator (op, "+", left, right) }
  | left = expression op = MINUS right = expression { Ir.Operator (op, "-", left, right) }
  | left = expression op = STAR right = expression { Ir.Operator (op, "*", left, right) }
  | left = expression op = DIVIDE right = expression { Ir.Operator (op, "/", left, right) }
  | PARENTHL ex = expression PARENTHR { ex }
  | t = atom { t }
atom:
  | n = NUMBER { Ir.Number (Tuple2.get1 n, Tuple2.get2 n) }
  | n = IDENTIFIER { Ir.Identifier (Tuple2.get1 n, Tuple2.get2 n) }
variable:
  | id = IDENTIFIER { Tuple2.get2 id }
  