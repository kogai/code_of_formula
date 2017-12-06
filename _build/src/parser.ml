
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | PARENTHR of (
# 11 "src/parser.mly"
       (Ir.info)
# 11 "src/parser.ml"
  )
    | PARENTHL of (
# 10 "src/parser.mly"
       (Ir.info)
# 16 "src/parser.ml"
  )
    | OPERATOR of (
# 8 "src/parser.mly"
       (Ir.info * string)
# 21 "src/parser.ml"
  )
    | NUMBER of (
# 7 "src/parser.mly"
       (Ir.info * float)
# 26 "src/parser.ml"
  )
    | IDENTIFIER of (
# 6 "src/parser.mly"
       (Ir.info * string)
# 31 "src/parser.ml"
  )
    | EQUAL of (
# 12 "src/parser.mly"
       (Ir.info)
# 36 "src/parser.ml"
  )
    | EOF of (
# 14 "src/parser.mly"
       (Ir.info)
# 41 "src/parser.ml"
  )
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState5
  | MenhirState0

# 1 "src/parser.mly"
  
  open Ir
  open Core

# 67 "src/parser.ml"

let rec _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state * (Ir.t) -> (
# 8 "src/parser.mly"
       (Ir.info * string)
# 72 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NUMBER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
       (Ir.info * float)
# 100 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "src/parser.mly"
       (Ir.info * float)
# 108 "src/parser.ml"
    )) = _v in
    let _v : (Ir.t) = 
# 23 "src/parser.mly"
               ( Ir.Number (Tuple2.get1 n, Tuple2.get2 n) )
# 113 "src/parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OPERATOR _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | OPERATOR _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 15 "src/parser.mly"
       (Ir.t option)
# 159 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (
# 14 "src/parser.mly"
       (Ir.info)
# 180 "src/parser.ml"
        )) = _v in
        let _v : (
# 15 "src/parser.mly"
       (Ir.t option)
# 185 "src/parser.ml"
        ) = 
# 19 "src/parser.mly"
        ( None )
# 189 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (
# 15 "src/parser.mly"
       (Ir.t option)
# 196 "src/parser.ml"
        )) = _v in
        Obj.magic _1
    | NUMBER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/Users/kogaishinichi/.opam/4.03.0/lib/menhir/standard.mly"
  


# 210 "src/parser.ml"
