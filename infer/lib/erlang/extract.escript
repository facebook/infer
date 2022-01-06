#!/usr/bin/env escript
% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-mode(compile).

usage() ->
    Usage =
        [
            "Usage:",
            "    extract.escript <PATH> <OUTDIR>",
            "    extract.escript <TERM>",
            "",
            "In the first form, the script will traverse all beam files",
            "in PATH, read their Abstract Syntax Trees (AST), and write a",
            "corresponding file in OUTDIR with the AST encoded as json.",
            "PATH can be;",
            "  a directory, a beam file, or a file with a list of beam files.",
            "",
            "In the second form, intended for debugging, the string TERM will",
            "be converted to an Erlang term and rendered as json."
        ],
    lists:foreach(fun(U) -> io:format("~s~n", [U]) end, Usage),
    halt(1).

main([Str]) when is_integer(hd(Str)) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ [$.]),
    {ok, Term} = erl_parse:parse_term(Tokens),
    io:fwrite("~s~n", [ast_to_json(Term)]);
main([Path, OutDir]) ->
    filelib:ensure_dir(filename:join(OutDir, dummy)),
    ast_to_json(Path, OutDir);
main(_) ->
    usage().

ast_to_json(Path, OutDir) ->
    Beams = path_to_beams(Path),
    filelib:ensure_dir(filename:join(OutDir, dummy)),
    gather(mapper(OutDir, Beams)).

path_to_beams(Path) ->
    case filelib:is_dir(Path) of
        true ->
            filelib:wildcard(filename:join(Path, "*.beam"));
        false ->
            case lists:suffix(".beam", Path) of
                true ->
                    [Path];
                false ->
                    case file:consult(Path) of
                        {ok, Paths} ->
                            Paths;
                        E ->
                            io:format("error reading: ~s: ~p~n", [Path, E]),
                            halt(1)
                    end
            end
    end.

mapper(OutDir, Beams) ->
    Handler = fun(B) -> fun() -> exit(process_beam(B, OutDir)) end end,
    Spawner = fun(B) -> spawn_monitor(Handler(B)) end,
    maps:from_list(lists:map(Spawner, Beams)).

gather(Procs) when map_size(Procs) == 0 ->
    ok;
gather(Procs) ->
    receive
        {'DOWN', _Ref, process, Pid, R} ->
            maybe_err(R),
            gather(maps:remove(Pid, Procs))
    end.

%% `process_beam` will return `ok` on success.
maybe_err(ok) ->
    ok;
maybe_err({C, {R, [{M, F, Args, Info} | _Stack]}}) when is_list(Args) ->
    maybe_err({C, {R, [{M, F, length(Args), Info}]}});
maybe_err(R) ->
    io:fwrite("error: ~p~n", [R]).

%% this runs in its own process
process_beam(BeamFilePath, OutDir) ->
    case get_ast(BeamFilePath) of
        {ok, Module, Forms} ->
            OutFilePath = filename:join(OutDir, Module ++ ".json"),
            JSON = ast_to_json(Forms),
            ok = file:write_file(OutFilePath, JSON);
        {error, E} ->
            exit(#{BeamFilePath => E})
    end.

get_ast(BeamFilePath) ->
    case beam_lib:chunks(BeamFilePath, [abstract_code]) of
        {ok, {Module, [{abstract_code, {_, Forms}}]}} ->
            {ok, atom_to_list(Module), Forms};
        {error, _, What} ->
            {error, What}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% json encoder

ast_to_json(Term) ->
    unicode:characters_to_binary(emit(Term)).

%% values
emit(true) ->
    "true";
emit(false) ->
    "false";
%% primitive types
emit(I) when is_integer(I) ->
    integer_to_list(I);
emit(F) when is_float(F) ->
    float_to_list(F);
emit(A) when is_atom(A) ->
    emit_string(atom_to_list(A));
%% strings
emit(L) when is_integer(hd(L)) ->
    emit_string_or_list(L);
%% lists
emit(L) when is_list(L) ->
    emit_list(L);
% tuples
emit(T) when is_tuple(T) ->
    emit_list(tuple_to_list(T)).

emit_list(L) ->
    emit_array(lists:map(fun(X) -> emit(X) end, L)).

emit_array(L) ->
    [$[, string:join(L, ","), $]].

%% argument is a list that might be a string. if it's not, we
%% backtrack and render it as a list.
emit_string_or_list(L) ->
    try
        emit_string(L)
    catch
        _:_ -> emit_list(L)
    end.

%% Return a string compliant with ECMA-404, where all code points
%% above 126 are '\u' escaped.
emit_string(X) ->
    [$", sanitize(unicode:characters_to_list(X), []), $"].

%% input and output is a list of unicode codepoints (int). We are
%% guaranteed that 0 =< C =< 0x10FFFF (by the unicode standard,
%% enforced by 'unicode:characters_to_list'). We turn codepoints 8, 9,
%% 10, 12, 13, 34, and 92 into '\b', '\t', '\n', '\f', '\r', '\"', and
%% '\\'. For codepoints > 0xFFFF we use 'utf-16 surrogate
%% pairs'. Otherwise, for codepoints > 126 or < 32 we use
%% "\u<hex>". The rest is good old ASCII.
sanitize([C | Cs], O) when 126 < C -> sanitize(Cs, [hex(C) | O]);
sanitize([92 | Cs], O) -> sanitize(Cs, [$\\, $\\ | O]);
sanitize([34 | Cs], O) -> sanitize(Cs, [$", $\\ | O]);
sanitize([C | Cs], O) when 31 < C -> sanitize(Cs, [C | O]);
sanitize([8 | Cs], O) -> sanitize(Cs, [$b, $\\ | O]);
sanitize([9 | Cs], O) -> sanitize(Cs, [$t, $\\ | O]);
sanitize([10 | Cs], O) -> sanitize(Cs, [$n, $\\ | O]);
sanitize([12 | Cs], O) -> sanitize(Cs, [$f, $\\ | O]);
sanitize([13 | Cs], O) -> sanitize(Cs, [$r, $\\ | O]);
sanitize([C | Cs], O) -> sanitize(Cs, [hex(C) | O]);
sanitize([], O) -> lists:reverse(O).

%% "[A] code point that is not in the Basic Multilingual Plane [ >
%% 0xFFFF ], [...] may be represented as a twelve-character
%% sequence, encoding the UTF-16 surrogate pair corresponding to the
%% code point"
hex(C) when 16#FFFF < C ->
    %% unicode as utf-16 surrogate pairs
    High = 16#D800 + ((C - 16#10000) bsr 10),
    Low = 16#DC00 + (16#3FF band C),
    [hex(High), hex(Low)];
hex(C) ->
    %% unicode as 4-digit hex
    [
        $\\,
        $u,
        hex(12, C),
        hex(8, C),
        hex(4, C),
        hex(0, C)
    ].

%% C = 10 should map to $a, so we do C-10+$a = C-$W
hex(Idx, C) ->
    case 16#F band (C bsr Idx) of
        Digit when Digit < 10 -> Digit + $0;
        Char -> Char + $W
    end.
