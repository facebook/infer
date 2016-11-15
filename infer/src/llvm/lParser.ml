
exception Error

let _eRR =
  Error

type token = 
  | XOR
  | X86_FP80
  | X
  | VOID
  | UREM
  | UDIV
  | TRIPLE
  | TARGET
  | SUB
  | STORE
  | STAR
  | SREM
  | SHL
  | SDIV
  | RSQBRACK
  | RPAREN
  | RET
  | RBRACE
  | RANGLE
  | PPC_FP128
  | OR
  | NUW
  | NUMBERED_METADATA of (int)
  | NUMBERED_LOCAL of (int)
  | NUMBERED_GLOBAL of (int)
  | NULL
  | NSZ
  | NSW
  | NNAN
  | NINF
  | NAMED_METADATA of (string)
  | NAMED_LOCAL of (string)
  | NAMED_GLOBAL of (string)
  | MUL
  | METADATA_STRING of (string)
  | METADATA_NODE_BEGIN
  | METADATA_LOCATION
  | METADATA
  | LSQBRACK
  | LSHR
  | LPAREN
  | LOAD
  | LBRACE
  | LANGLE
  | LABEL
  | INT of (int)
  | INSERTELEMENT
  | IDENT of (string)
  | HALF
  | FSUB
  | FREM
  | FP128
  | FMUL
  | FLOAT
  | FDIV
  | FAST
  | FADD
  | EXTRACTELEMENT
  | EXACT
  | EQUALS
  | EOF
  | DOUBLE
  | DEFINE
  | DEBUG_ANNOTATION
  | DBG_DECLARE
  | DATALAYOUT
  | CONSTANT_STRING of (string)
  | CONSTANT_INT of (int)
  | COMMA
  | COLON
  | CALL
  | BR
  | ATTRIBUTE_GROUP of (int)
  | ASHR
  | ARCP
  | AND
  | ALLOCA
  | ALIGN
  | ADD

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState276
  | MenhirState272
  | MenhirState270
  | MenhirState266
  | MenhirState260
  | MenhirState258
  | MenhirState238
  | MenhirState236
  | MenhirState234
  | MenhirState230
  | MenhirState227
  | MenhirState225
  | MenhirState217
  | MenhirState214
  | MenhirState213
  | MenhirState211
  | MenhirState210
  | MenhirState208
  | MenhirState206
  | MenhirState205
  | MenhirState200
  | MenhirState198
  | MenhirState197
  | MenhirState195
  | MenhirState194
  | MenhirState192
  | MenhirState191
  | MenhirState189
  | MenhirState188
  | MenhirState186
  | MenhirState185
  | MenhirState183
  | MenhirState182
  | MenhirState180
  | MenhirState179
  | MenhirState177
  | MenhirState176
  | MenhirState174
  | MenhirState163
  | MenhirState161
  | MenhirState160
  | MenhirState158
  | MenhirState157
  | MenhirState155
  | MenhirState154
  | MenhirState152
  | MenhirState151
  | MenhirState150
  | MenhirState148
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState141
  | MenhirState140
  | MenhirState138
  | MenhirState137
  | MenhirState135
  | MenhirState133
  | MenhirState128
  | MenhirState126
  | MenhirState124
  | MenhirState122
  | MenhirState119
  | MenhirState117
  | MenhirState116
  | MenhirState112
  | MenhirState109
  | MenhirState106
  | MenhirState104
  | MenhirState95
  | MenhirState93
  | MenhirState90
  | MenhirState89
  | MenhirState85
  | MenhirState82
  | MenhirState77
  | MenhirState75
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState55
  | MenhirState52
  | MenhirState46
  | MenhirState38
  | MenhirState30
  | MenhirState29
  | MenhirState27
  | MenhirState19
  | MenhirState16
  | MenhirState9
  | MenhirState8
  | MenhirState0
  
  open LAst

let rec _menhir_goto_option_FAST_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_5 : (unit option)) = _v in
    let ((((_menhir_stack, _menhir_s, (_1 : (unit option))), (_2 : (unit option))), (_3 : (unit option))), (_4 : (unit option))) = _menhir_stack in
    let _v : (unit) =                                  ( () ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState186
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_ARCP_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FAST ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_FAST_ _menhir_env _menhir_stack _v
    | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_FAST_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_annotated_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.annotated_instruction option list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (annotated_instrs : (LAst.annotated_instruction option list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (LAst.annotated_instruction list) =                                                             ( IList.flatten_options annotated_instrs ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (annotated_instrs : (LAst.annotated_instruction list)) = _v in
            let (((((_menhir_stack, _menhir_s), _, (ret_tp : (LAst.typ option))), _, (name : (LAst.variable))), _, (xs0 : ((LAst.typ * string) list))), _, (_7 : (int list))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (LAst.function_def) = let params =
              let xs = xs0 in
                  ( xs )
            in
                                         ( FunctionDef (name, ret_tp, params, annotated_instrs) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DEFINE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState270
            | EOF | NAMED_METADATA _ | NUMBERED_METADATA _ ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState270
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (LAst.annotated_instruction option))), _, (xs : (LAst.annotated_instruction option list))) = _menhir_stack in
        let _v : (LAst.annotated_instruction option list) =     ( x :: xs ) in
        _menhir_goto_list_annotated_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_NSZ_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARCP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_ARCP_ _menhir_env _menhir_stack _v
    | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_ARCP_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_annotated_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.annotated_instruction option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BR ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState227
    | CALL ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState227
    | NAMED_GLOBAL _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
    | NAMED_LOCAL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
    | NUMBERED_GLOBAL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
    | NUMBERED_LOCAL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
    | RET ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState227
    | STORE ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState227
    | RBRACE ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState227
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState227

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (LAst.annotated_instruction option list) =     ( [] ) in
    _menhir_goto_list_annotated_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FLOAT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FP128 ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | HALF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LABEL ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LANGLE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LSQBRACK ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | METADATA ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PPC_FP128 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | VOID ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | X86_FP80 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FLOAT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FP128 ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | HALF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LABEL ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LANGLE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LSQBRACK ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | METADATA ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PPC_FP128 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | VOID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState75 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack)
        | BR | CALL | COMMA | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STORE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (LAst.instruction) =              ( Ret None ) in
            _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | X86_FP80 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VOID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DBG_DECLARE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | METADATA ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | CONSTANT_INT _ | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA_NODE_BEGIN | METADATA_STRING _ | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | NUMBERED_METADATA _ | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | RPAREN ->
                    _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | LABEL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_NINF_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NSZ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_NSZ_ _menhir_env _menhir_stack _v
    | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_NSZ_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_NSW_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (unit option)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (unit option))) = _menhir_stack in
    let _v : (unit) =               ( () ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_annotation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.annotation option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (anno : (LAst.annotation option)) = _v in
        let (_menhir_stack, _menhir_s, (instr : (LAst.instruction))) = _menhir_stack in
        let _v : (LAst.annotated_instruction option) =                                                 ( Some (instr, anno) ) in
        _menhir_goto_annotated_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (LAst.annotation option)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
        let _v : (LAst.annotated_instruction option) =                                   ( None ) in
        _menhir_goto_annotated_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_metadata_component_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.metadata_component list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState93 | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (LAst.metadata_component list)) = _v in
        let _v : (LAst.metadata_component list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_metadata_component__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (LAst.metadata_component list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (LAst.metadata_component))) = _menhir_stack in
        let _2 = () in
        let _v : (LAst.metadata_component list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_metadata_component_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_metadata_component__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.metadata_component list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (unit option))), _, (xs0 : (LAst.metadata_component list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (LAst.metadata_component list) = let components =
              let xs = xs0 in
                  ( xs )
            in
                                                                                                            ( components ) in
            (match _menhir_s with
            | MenhirState85 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (components : (LAst.metadata_component list)) = _v in
                let _v : (LAst.metadata_value) =                                ( MetadataNode components ) in
                _menhir_goto_metadata_value _menhir_env _menhir_stack _menhir_s _v
            | MenhirState238 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (components : (LAst.metadata_component list)) = _v in
                let _v : (LAst.metadata_aggregate) =                                ( Components components ) in
                _menhir_goto_metadata_aggregate _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs0 : (LAst.metadata_component list))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (unit) = let _5 =
              let xs = xs0 in
                  ( xs )
            in
                                                                                              ( () ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                _menhir_run218 _menhir_env (Obj.magic _menhir_stack) MenhirState225
            | BR | CALL | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STORE ->
                _menhir_reduce102 _menhir_env (Obj.magic _menhir_stack) MenhirState225
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState225)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_attribute_group_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (int list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BR ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | CALL ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NAMED_GLOBAL _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | NAMED_LOCAL _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | NUMBERED_GLOBAL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | NUMBERED_LOCAL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | RET ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | STORE ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | RBRACE ->
                _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (int))), _, (xs : (int list))) = _menhir_stack in
        let _v : (int list) =     ( x :: xs ) in
        _menhir_goto_list_attribute_group_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state * (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (tp : (LAst.typ))) = _menhir_stack in
    let _v : (LAst.typ) =                  ( Tptr tp ) in
    _menhir_goto_element_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_NNAN_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NINF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_NINF_ _menhir_env _menhir_stack _v
    | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NSZ | PPC_FP128 | VOID | X86_FP80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_NINF_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_EXACT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | METADATA ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_NUW_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NSW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = () in
        let _v : (unit option) =     ( Some x ) in
        _menhir_goto_option_NSW_ _menhir_env _menhir_stack _v
    | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (unit option) =     ( None ) in
        _menhir_goto_option_NSW_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (LAst.annotation option) =     ( None ) in
    _menhir_goto_option_annotation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run218 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEBUG_ANNOTATION ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NUMBERED_METADATA _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (int)) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (LAst.annotation) =                                                  ( Annotation i ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (LAst.annotation)) = _v in
            let _v : (LAst.annotation option) =     ( Some x ) in
            _menhir_goto_option_annotation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_align_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (int option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_7 : (int option)) = _v in
        let (((((_menhir_stack, _menhir_s), _, (val_tp : (LAst.typ))), _, (value : (LAst.operand))), _, (_ptr_tp : (LAst.typ))), _, (var : (LAst.variable))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (LAst.instruction) =       ( Store (value, val_tp, var) ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_6 : (int option)) = _v in
        let (((_menhir_stack, _menhir_s, (var : (LAst.variable))), _, (tp : (LAst.typ))), _, (ptr : (LAst.variable))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (LAst.instruction) =                                                                   ( Load (var, tp, ptr) ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_5 : (int option)) = _v in
        let ((_menhir_stack, _menhir_s, (var : (LAst.variable))), _, (tp : (LAst.typ))) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _v : (LAst.instruction) =                                                  ( Alloc (var, tp, 1) ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_pair_typ_operand__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((LAst.typ * LAst.operand) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((LAst.typ * LAst.operand) list)) = _v in
        let ((_menhir_stack, _menhir_s, (x0 : (LAst.typ))), _, (y0 : (LAst.operand))) = _menhir_stack in
        let _2 = () in
        let _v : ((LAst.typ * LAst.operand) list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_pair_typ_operand__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((LAst.typ * LAst.operand) list)) = _v in
        let _v : ((LAst.typ * LAst.operand) list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_pair_typ_operand___ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_3 : (unit)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (LAst.variable))) = _menhir_stack in
    let _2 = () in
    let _v : (LAst.instruction) =                           ( Binop ) in
    _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_metadata_component : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.metadata_component) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METADATA ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | CONSTANT_INT _ | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA_NODE_BEGIN | METADATA_STRING _ | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | NUMBERED_METADATA _ | PPC_FP128 | VOID | X86_FP80 ->
            _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | RBRACE | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (LAst.metadata_component))) = _menhir_stack in
        let _v : (LAst.metadata_component list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_metadata_component_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (LAst.metadata_component list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_metadata_component__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_metadata_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.metadata_value) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (value : (LAst.metadata_value)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (unit option))) = _menhir_stack in
    let _v : (LAst.metadata_component) =                                      ( MetadataVal value ) in
    _menhir_goto_metadata_component _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (int list) =     ( [] ) in
    _menhir_goto_list_attribute_group_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (int) =                         ( i ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATTRIBUTE_GROUP _v ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
    | LBRACE ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState230
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230

and _menhir_goto_option_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONSTANT_INT _v ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NAMED_GLOBAL _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NAMED_LOCAL _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NUMBERED_GLOBAL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NUMBERED_LOCAL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (LAst.typ) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (tp : (LAst.typ))) = _menhir_stack in
    let _2 = () in
    let _v : (LAst.typ) =                   ( tp ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 | MenhirState234 | MenhirState214 | MenhirState210 | MenhirState208 | MenhirState206 | MenhirState194 | MenhirState197 | MenhirState200 | MenhirState191 | MenhirState186 | MenhirState183 | MenhirState180 | MenhirState177 | MenhirState174 | MenhirState157 | MenhirState160 | MenhirState148 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState135 | MenhirState133 | MenhirState126 | MenhirState122 | MenhirState116 | MenhirState85 | MenhirState75 | MenhirState59 | MenhirState52 | MenhirState16 | MenhirState19 | MenhirState38 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | LPAREN | STAR ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
        | LPAREN | STAR ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_pair_typ_operand___ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((LAst.typ * LAst.operand) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, (ret_var : (LAst.variable))), _, (_4 : (LAst.typ option))), _, (func_var : (LAst.variable))), _, (xs0 : ((LAst.typ * LAst.operand) list))) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _3 = () in
        let _2 = () in
        let _v : (LAst.instruction) = let args =
          let xs = xs0 in
              ( xs )
        in
                                                                    ( Call (ret_var, func_var, args) ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_NNAN_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run164 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_NNAN_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_EXACT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_EXACT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_NUW_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_NUW_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_real_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        _menhir_run218 _menhir_env (Obj.magic _menhir_stack) MenhirState217
    | BR | CALL | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STORE ->
        _menhir_reduce102 _menhir_env (Obj.magic _menhir_stack) MenhirState217
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState217

and _menhir_reduce100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (int option) =     ( None ) in
    _menhir_goto_option_align_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ALIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (width : (int)) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (int) =                                      ( width ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (int)) = _v in
            let _v : (int option) =     ( Some x ) in
            _menhir_goto_option_align_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_operand : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.operand) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (tp : (LAst.typ))), _, (op : (LAst.operand))) = _menhir_stack in
        let _1 = () in
        let _v : (LAst.instruction) =                               ( Ret (Some (tp, op)) ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (unit option))), _, (tp : (LAst.typ option))), _, (op : (LAst.operand))) = _menhir_stack in
        let _v : (LAst.metadata_component) =                                      ( TypOperand (tp, op) ) in
        _menhir_goto_metadata_component _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LABEL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NAMED_GLOBAL _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                | NAMED_LOCAL _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                | NUMBERED_GLOBAL _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                | NUMBERED_LOCAL _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONSTANT_INT _v ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | NAMED_GLOBAL _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | NAMED_LOCAL _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NUMBERED_GLOBAL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | NUMBERED_LOCAL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (LAst.typ))), _, (_2 : (LAst.operand))), _, (_4 : (LAst.operand))) = _menhir_stack in
        let _3 = () in
        let _v : (unit) =                               ( () ) in
        (match _menhir_s with
        | MenhirState116 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let _1 = () in
            let _v : (unit) =                    ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState122 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let _1 = () in
            let _v : (unit) =                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState126 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit option))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                            ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState133 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                  ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState135 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let _1 = () in
            let _v : (unit) =                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState138 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                  ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState141 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit option))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                            ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState143 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let _1 = () in
            let _v : (unit) =                   ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState145 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let _1 = () in
            let _v : (unit) =                    ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState148 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit option))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                            ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState174 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState177 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState180 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState183 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState186 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                     ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState206 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit option))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                            ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState208 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let _1 = () in
            let _v : (unit) =                    ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | MenhirState214 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
            let _1 = () in
            let _v : (unit) =                                  ( () ) in
            _menhir_goto_binop _menhir_env _menhir_stack _v
        | _ ->
            _menhir_fail ())
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _, (_2 : (LAst.typ))), _, (_3 : (LAst.operand))), _, (_5 : (LAst.typ))), _, (_6 : (LAst.operand))), _, (_8 : (LAst.typ))), _, (_9 : (LAst.operand))) = _menhir_stack in
        let _7 = () in
        let _4 = () in
        let _1 = () in
        let _v : (unit) =                                                                          ( () ) in
        _menhir_goto_binop _menhir_env _menhir_stack _v
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState191
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _, (_2 : (LAst.typ))), _, (_3 : (LAst.operand))), _, (_5 : (LAst.typ))), _, (_6 : (LAst.operand))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (unit) =                                                         ( () ) in
        _menhir_goto_binop _menhir_env _menhir_stack _v
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState200
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (x0 : (LAst.typ))), _, (y0 : (LAst.operand))) = _menhir_stack in
            let _v : ((LAst.typ * LAst.operand) list) = let x =
              let y = y0 in
              let x = x0 in
                  ( (x, y) )
            in
                ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_pair_typ_operand__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_METADATA_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState95 | MenhirState93 | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | FLOAT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | FP128 ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | HALF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | LABEL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LANGLE ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LSQBRACK ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | METADATA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METADATA_NODE_BEGIN ->
                _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack)
            | CONSTANT_INT _ | LPAREN | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | STAR ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | METADATA_STRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (str : (string)) = _v in
            let _v : (LAst.metadata_value) =                           ( MetadataString str ) in
            _menhir_goto_metadata_value _menhir_env _menhir_stack _menhir_s _v
        | NUMBERED_METADATA _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (int)) = _v in
            let _v : (LAst.metadata_value) =                           ( MetadataVar i ) in
            _menhir_goto_metadata_value _menhir_env _menhir_stack _menhir_s _v
        | PPC_FP128 ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | VOID ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | X86_FP80 ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | CONSTANT_INT _ | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _v : (LAst.typ option) =     ( None ) in
            _menhir_goto_option_typ_ _menhir_env _menhir_stack _menhir_s _v
        | METADATA_NODE_BEGIN ->
            _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState238 | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METADATA_NODE_BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METADATA ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | CONSTANT_INT _ | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA_NODE_BEGIN | METADATA_STRING _ | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | NUMBERED_METADATA _ | PPC_FP128 | VOID | X86_FP80 ->
                _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | RBRACE ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce89 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let x = () in
    let _v : (unit option) =     ( Some x ) in
    _menhir_goto_option_METADATA_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_metadata_def : _menhir_env -> 'ttv_tail -> _menhir_state -> ((int * LAst.metadata_aggregate) option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | NAMED_METADATA _v ->
        _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | NUMBERED_METADATA _v ->
        _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState266
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266

and _menhir_goto_separated_nonempty_list_COMMA_NUMBERED_METADATA_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (int list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (int list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (int))) = _menhir_stack in
        let _2 = () in
        let _v : (int list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_NUMBERED_METADATA_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (int list)) = _v in
        let _v : (int list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_NUMBERED_METADATA__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (LAst.typ option) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FLOAT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FP128 ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | HALF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LABEL ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LANGLE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LSQBRACK ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | METADATA ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PPC_FP128 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VOID ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | X86_FP80 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState30 in
        let _v : (LAst.typ list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_first_class_typ__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_goto_loption_separated_nonempty_list_COMMA_pair_first_class_typ_IDENT___ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((LAst.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ATTRIBUTE_GROUP _v ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LBRACE ->
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_first_class_typ__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (ret_tp : (LAst.typ option))), _), _, (xs0 : (LAst.typ list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (LAst.typ) = let param_tps =
          let xs = xs0 in
              ( xs )
        in
                                                                                              ( Tfunc (ret_tp, param_tps) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (tp : (LAst.typ)) = _v in
        let _v : (LAst.typ) =                   ( tp ) in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState234 | MenhirState194 | MenhirState150 | MenhirState65 | MenhirState52 | MenhirState9 | MenhirState16 | MenhirState38 | MenhirState30 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | CONSTANT_INT _ | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (LAst.typ))) = _menhir_stack in
            let _v : (LAst.typ option) =     ( Some x ) in
            _menhir_goto_option_typ_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState214 | MenhirState208 | MenhirState206 | MenhirState186 | MenhirState183 | MenhirState180 | MenhirState177 | MenhirState174 | MenhirState148 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState135 | MenhirState133 | MenhirState126 | MenhirState122 | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
    | MenhirState200 | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_INT _v ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | STAR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | BR | CALL | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STORE ->
            _menhir_reduce100 _menhir_env (Obj.magic _menhir_stack) MenhirState211
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211)
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.variable) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState52 in
                let _v : ((LAst.typ * string) list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_pair_first_class_typ_IDENT___ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState198 | MenhirState192 | MenhirState189 | MenhirState161 | MenhirState158 | MenhirState155 | MenhirState119 | MenhirState117 | MenhirState106 | MenhirState90 | MenhirState77 | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (var : (LAst.variable))) = _menhir_stack in
        let _v : (LAst.operand) =                    ( Var var ) in
        _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BR | CALL | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STORE ->
            _menhir_reduce100 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (lbl : (LAst.variable))) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (LAst.instruction) =                             ( UncondBranch lbl ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LABEL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NAMED_GLOBAL _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | NAMED_LOCAL _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | NUMBERED_GLOBAL _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | NUMBERED_LOCAL _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), (_ : (int))), _, (op : (LAst.operand))), _, (lbl1 : (LAst.variable))), _, (lbl2 : (LAst.variable))) = _menhir_stack in
        let _8 = () in
        let _7 = () in
        let _5 = () in
        let _4 = () in
        let _1 = () in
        let _v : (LAst.instruction) =       ( CondBranch (op, lbl1, lbl2) ) in
        _menhir_goto_real_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState227 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADD ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NUW ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState213
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NSW | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack) MenhirState213
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213)
            | ALLOCA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState210
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
            | AND ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState208
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
            | ASHR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXACT ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
            | CALL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
            | EXTRACTELEMENT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState188
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
            | FADD ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NNAN ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState185
                | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NINF | NSZ | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState185
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
            | FDIV ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NNAN ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState182
                | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NINF | NSZ | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState182
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
            | FMUL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NNAN ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState179
                | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NINF | NSZ | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState179
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179)
            | FREM ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NNAN ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NINF | NSZ | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState176
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
            | FSUB ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NNAN ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | ARCP | DOUBLE | FAST | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NINF | NSZ | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
            | INSERTELEMENT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState154
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
            | LOAD ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState150
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
            | LSHR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXACT ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
            | MUL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
            | OR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
            | SDIV ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXACT ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
            | SHL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NUW ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NSW | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack) MenhirState137
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
            | SREM ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState135
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
            | SUB ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NUW ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState128
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | NSW | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack) MenhirState128
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
            | UDIV ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EXACT ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | DOUBLE | FLOAT | FP128 | HALF | INT _ | LABEL | LANGLE | LSQBRACK | METADATA | PPC_FP128 | VOID | X86_FP80 ->
                    _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
            | UREM ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
            | XOR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | BR | CALL | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STORE ->
            _menhir_reduce100 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState197
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState197 in
                let _v : ((LAst.typ * LAst.operand) list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_pair_typ_operand___ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_constant : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.constant) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (const : (LAst.constant)) = _v in
    let _v : (LAst.operand) =                      ( Const const ) in
    _menhir_goto_operand _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_metadata_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((int * LAst.metadata_aggregate) option list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : ((int * LAst.metadata_aggregate) option))), _, (xs : ((int * LAst.metadata_aggregate) option list))) = _menhir_stack in
        let _v : ((int * LAst.metadata_aggregate) option list) =     ( x :: xs ) in
        _menhir_goto_list_metadata_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (string option * string option))), _, (func_defs : (LAst.function_def list))), _, (opt_mappings : ((int * LAst.metadata_aggregate) option list))) = _menhir_stack in
            let _4 = () in
            let _v : (LAst.program) =                                                                        (
      let mappings = IList.flatten_options opt_mappings in
      let add_mapping map (metadata_id, aggregate) = MetadataMap.add metadata_id aggregate map in
      let metadata_map = IList.fold_left add_mapping MetadataMap.empty mappings in
      Program (func_defs, metadata_map) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (LAst.program)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) =     ( None ) in
    _menhir_goto_option_METADATA_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_metadata_aggregate : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.metadata_aggregate) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (aggregate : (LAst.metadata_aggregate)) = _v in
    let (_menhir_stack, _menhir_s, (metadata_id : (int))) = _menhir_stack in
    let _2 = () in
    let _v : ((int * LAst.metadata_aggregate) option) =                                                                           ( Some
  (metadata_id, aggregate) ) in
    _menhir_goto_metadata_def _menhir_env _menhir_stack _menhir_s _v

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_loption_separated_nonempty_list_COMMA_NUMBERED_METADATA__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (int list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (xs0 : (int list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (int list) = let metadata_ids =
          let xs = xs0 in
              ( xs )
        in
                                                                                               ( metadata_ids ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (int list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (string))) = _menhir_stack in
        let _2 = () in
        let _v : ((int * LAst.metadata_aggregate) option) =                                                  ( None ) in
        _menhir_goto_metadata_def _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run259 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NUMBERED_METADATA _v ->
            _menhir_run259 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState260)
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (int))) = _menhir_stack in
        let _v : (int list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_NUMBERED_METADATA_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_ret_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState234 | MenhirState214 | MenhirState210 | MenhirState208 | MenhirState206 | MenhirState197 | MenhirState200 | MenhirState191 | MenhirState186 | MenhirState183 | MenhirState180 | MenhirState177 | MenhirState174 | MenhirState157 | MenhirState160 | MenhirState150 | MenhirState148 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState135 | MenhirState133 | MenhirState126 | MenhirState122 | MenhirState116 | MenhirState85 | MenhirState75 | MenhirState59 | MenhirState65 | MenhirState52 | MenhirState16 | MenhirState38 | MenhirState30 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | NAMED_GLOBAL _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | NAMED_LOCAL _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | NUMBERED_GLOBAL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | NUMBERED_LOCAL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_pair_first_class_typ_IDENT__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((LAst.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((LAst.typ * string) list)) = _v in
        let _v : ((LAst.typ * string) list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_pair_first_class_typ_IDENT___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState234 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((LAst.typ * string) list)) = _v in
        let ((_menhir_stack, _menhir_s, (x0 : (LAst.typ))), (y0 : (string))) = _menhir_stack in
        let _2 = () in
        let _v : ((LAst.typ * string) list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_pair_first_class_typ_IDENT__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce118 : _menhir_env -> 'ttv_tail * _menhir_state * (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (tp : (LAst.typ))) = _menhir_stack in
    let _v : (LAst.typ option) =                          ( Some tp ) in
    _menhir_goto_ret_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_first_class_typ_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (LAst.typ list)) = _v in
        let _v : (LAst.typ list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_first_class_typ__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (LAst.typ list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (LAst.typ))) = _menhir_stack in
        let _2 = () in
        let _v : (LAst.typ list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_first_class_typ_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce136 : _menhir_env -> 'ttv_tail * _menhir_state * (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (tp : (LAst.typ))) = _menhir_stack in
    let _v : (LAst.typ) =                          ( tp ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (num : (int)) = _v in
    let _v : (LAst.variable) =                          ( Local (Number num) ) in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (num : (int)) = _v in
    let _v : (LAst.variable) =                           ( Global (Number num) ) in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (LAst.constant) =          ( Cnull ) in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (name : (string)) = _v in
    let _v : (LAst.variable) =                        ( Local (Name name) ) in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (name : (string)) = _v in
    let _v : (LAst.variable) =                         ( Global (Name name) ) in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (LAst.constant) =                      ( Cint i ) in
    _menhir_goto_constant _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce37 : _menhir_env -> 'ttv_tail * _menhir_state * (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (tp : (LAst.typ))) = _menhir_stack in
    let _v : (LAst.typ) =                      ( tp ) in
    _menhir_goto_first_class_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((int * LAst.metadata_aggregate) option list) =     ( [] ) in
    _menhir_goto_list_metadata_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run237 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METADATA ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | METADATA_LOCATION ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState238 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | CONSTANT_INT _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | COMMA ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | IDENT _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | COLON ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | CONSTANT_INT _v ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_stack = (_menhir_stack, _v) in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | COMMA ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | IDENT _v ->
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let _menhir_stack = (_menhir_stack, _v) in
                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                    let _tok = _menhir_env._menhir_token in
                                                    (match _tok with
                                                    | COLON ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _tok = _menhir_env._menhir_token in
                                                        (match _tok with
                                                        | NUMBERED_METADATA _v ->
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let _menhir_stack = (_menhir_stack, _v) in
                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                            let _tok = _menhir_env._menhir_token in
                                                            (match _tok with
                                                            | RPAREN ->
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let (((((((_menhir_stack, _menhir_s), (_3 : (string))), (line_num : (int))), (_7 : (string))), (col_num : (int))), (_11 : (string))), (i : (int))) = _menhir_stack in
                                                                let _14 = () in
                                                                let _12 = () in
                                                                let _10 = () in
                                                                let _8 = () in
                                                                let _6 = () in
                                                                let _4 = () in
                                                                let _2 = () in
                                                                let _1 = () in
                                                                let _v : (LAst.metadata_location) =     ( { line = line_num; col = col_num; scope = MetadataVar i} ) in
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let (location : (LAst.metadata_location)) = _v in
                                                                let _v : (LAst.metadata_aggregate) =                                  ( Location location ) in
                                                                _menhir_goto_metadata_aggregate _menhir_env _menhir_stack _menhir_s _v
                                                            | _ ->
                                                                assert (not _menhir_env._menhir_error);
                                                                _menhir_env._menhir_error <- true;
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let (((((((_menhir_stack, _menhir_s), _), _), _), _), _), _) = _menhir_stack in
                                                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let ((((((_menhir_stack, _menhir_s), _), _), _), _), _) = _menhir_stack in
                                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let ((((((_menhir_stack, _menhir_s), _), _), _), _), _) = _menhir_stack in
                                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let (((((_menhir_stack, _menhir_s), _), _), _), _) = _menhir_stack in
                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (((((_menhir_stack, _menhir_s), _), _), _), _) = _menhir_stack in
                                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | METADATA_NODE_BEGIN ->
            _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run256 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METADATA_NODE_BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NUMBERED_METADATA _v ->
                _menhir_run259 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState258 in
                let _v : (int list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_NUMBERED_METADATA__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce117 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : (LAst.typ option) =          ( None ) in
    _menhir_goto_ret_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce41 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : (LAst.typ) =              ( Tmetadata ) in
    _menhir_goto_first_class_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_first_class_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | STAR ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (LAst.typ))) = _menhir_stack in
            let _v : (LAst.typ list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_first_class_typ_ _menhir_env _menhir_stack _menhir_s _v
        | LPAREN ->
            _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 | MenhirState194 | MenhirState16 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STAR ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN | NAMED_GLOBAL _ | NAMED_LOCAL _ | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ ->
            _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState214 | MenhirState210 | MenhirState208 | MenhirState206 | MenhirState197 | MenhirState200 | MenhirState191 | MenhirState186 | MenhirState183 | MenhirState180 | MenhirState177 | MenhirState174 | MenhirState157 | MenhirState160 | MenhirState150 | MenhirState148 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState135 | MenhirState133 | MenhirState126 | MenhirState122 | MenhirState116 | MenhirState85 | MenhirState75 | MenhirState59 | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BR | CALL | COMMA | CONSTANT_INT _ | NAMED_GLOBAL _ | NAMED_LOCAL _ | NULL | NUMBERED_GLOBAL _ | NUMBERED_LOCAL _ | RBRACE | RET | STAR | STORE ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState234 | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOUBLE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | FLOAT ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | FP128 ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | HALF ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | INT _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
                | LABEL ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | LANGLE ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | LSQBRACK ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | METADATA ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | PPC_FP128 ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | VOID ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | X86_FP80 ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState234
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState234)
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (x0 : (LAst.typ))), (y0 : (string))) = _menhir_stack in
                let _v : ((LAst.typ * string) list) = let x =
                  let y = y0 in
                  let x = x0 in
                      ( (x, y) )
                in
                    ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_COMMA_pair_first_class_typ_IDENT__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_reduce136 _menhir_env (Obj.magic _menhir_stack)
        | LPAREN ->
            _menhir_reduce118 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_element_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 | MenhirState52 | MenhirState234 | MenhirState214 | MenhirState210 | MenhirState208 | MenhirState206 | MenhirState194 | MenhirState197 | MenhirState200 | MenhirState191 | MenhirState186 | MenhirState183 | MenhirState180 | MenhirState177 | MenhirState174 | MenhirState157 | MenhirState160 | MenhirState150 | MenhirState148 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState135 | MenhirState133 | MenhirState126 | MenhirState122 | MenhirState116 | MenhirState85 | MenhirState75 | MenhirState59 | MenhirState65 | MenhirState30 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (sz : (int))), _, (tp : (LAst.typ))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (LAst.typ) =                                                        ( Tvector (sz, tp) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState234 | MenhirState214 | MenhirState210 | MenhirState208 | MenhirState206 | MenhirState200 | MenhirState197 | MenhirState194 | MenhirState191 | MenhirState186 | MenhirState183 | MenhirState180 | MenhirState177 | MenhirState174 | MenhirState160 | MenhirState157 | MenhirState150 | MenhirState148 | MenhirState145 | MenhirState143 | MenhirState141 | MenhirState138 | MenhirState135 | MenhirState133 | MenhirState126 | MenhirState122 | MenhirState116 | MenhirState85 | MenhirState75 | MenhirState65 | MenhirState59 | MenhirState52 | MenhirState9 | MenhirState16 | MenhirState38 | MenhirState30 | MenhirState19 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (tp : (LAst.typ))) = _menhir_stack in
                let _v : (LAst.typ) =                     ( tp ) in
                _menhir_goto_first_class_typ _menhir_env _menhir_stack _menhir_s _v
            | MenhirState154 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONSTANT_INT _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                | NAMED_GLOBAL _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                | NAMED_LOCAL _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                | NULL ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                | NUMBERED_GLOBAL _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                | NUMBERED_LOCAL _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
            | MenhirState188 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CONSTANT_INT _v ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                | NAMED_GLOBAL _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                | NAMED_LOCAL _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                | NULL ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState189
                | NUMBERED_GLOBAL _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                | NUMBERED_LOCAL _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189)
            | _ ->
                _menhir_fail ())
        | LPAREN | STAR ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RSQBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (sz : (int))), _, (tp : (LAst.typ))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (LAst.typ) =                                                            ( Tarray (sz, tp) ) in
            _menhir_goto_first_class_typ _menhir_env _menhir_stack _menhir_s _v
        | LPAREN | STAR ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_floating_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (unit)) = _v in
    let _v : (LAst.typ) =                  ( Tfloat ) in
    _menhir_goto_element_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_function_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (LAst.function_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NAMED_METADATA _v ->
            _menhir_run256 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | NUMBERED_METADATA _v ->
            _menhir_run237 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | EOF ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236)
    | MenhirState270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (LAst.function_def))), _, (xs : (LAst.function_def list))) = _menhir_stack in
        let _v : (LAst.function_def list) =     ( x :: xs ) in
        _menhir_goto_list_function_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =              ( () ) in
    _menhir_goto_floating_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce117 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =               ( () ) in
    _menhir_goto_floating_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONSTANT_INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | X ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONSTANT_INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | X ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOUBLE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | FLOAT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | FP128 ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | HALF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LABEL ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LANGLE ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LSQBRACK ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | METADATA ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | PPC_FP128 ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | VOID ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | X86_FP80 ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (LAst.typ) =           ( Tlabel ) in
    _menhir_goto_first_class_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (width : (int)) = _v in
    let _v : (LAst.typ) =                 ( Tint width ) in
    _menhir_goto_element_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =          ( () ) in
    _menhir_goto_floating_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =           ( () ) in
    _menhir_goto_floating_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =           ( () ) in
    _menhir_goto_floating_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) =            ( () ) in
    _menhir_goto_floating_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (LAst.function_def list) =     ( [] ) in
    _menhir_goto_list_function_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | FLOAT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | FP128 ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | HALF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LABEL ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LANGLE ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LSQBRACK ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | METADATA ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | PPC_FP128 ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | VOID ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | X86_FP80 ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_targets : _menhir_env -> 'ttv_tail -> _menhir_state -> (string option * string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEFINE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | EOF | NAMED_METADATA _ | NUMBERED_METADATA _ ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState270 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState234 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState217 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run2 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_STRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (str : (string)) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (string) =                                                ( str ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState0 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TARGET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState272 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | DATALAYOUT ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | DEFINE | EOF | NAMED_METADATA _ | NUMBERED_METADATA _ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (tt : (string))) = _menhir_stack in
                    let _v : (string option * string option) =                        ( (None, Some tt) ) in
                    _menhir_goto_targets _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState272)
            | MenhirState276 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (dl : (string))), _, (tt : (string))) = _menhir_stack in
                let _v : (string option * string option) =                                        ( (Some dl, Some tt) ) in
                _menhir_goto_targets _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONSTANT_STRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (str : (string)) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (string) =                                                    ( str ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState272 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (tt : (string))), _, (dl : (string))) = _menhir_stack in
                let _v : (string option * string option) =                                        ( (Some dl, Some tt) ) in
                _menhir_goto_targets _menhir_env _menhir_stack _menhir_s _v
            | MenhirState0 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TARGET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState276 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | TRIPLE ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | DEFINE | EOF | NAMED_METADATA _ | NUMBERED_METADATA _ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (dl : (string))) = _menhir_stack in
                    let _v : (string option * string option) =                     ( (Some dl, None) ) in
                    _menhir_goto_targets _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState276)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (LAst.program) =
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
    | TARGET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DATALAYOUT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack)
        | TRIPLE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | DEFINE | EOF | NAMED_METADATA _ | NUMBERED_METADATA _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _v : (string option * string option) =     ( (None, None) ) in
        _menhir_goto_targets _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

