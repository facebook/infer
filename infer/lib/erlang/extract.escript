#!/usr/bin/env escript
% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-mode(compile).

-define(OTP_MODULES_LIST, "otp_modules.list").

usage() ->
    Usage =
        [
            "Usage:",
            "    extract.escript <BEAMS> [--specs-only <BEAMS>] <OUTDIR>",
            "",
            "For each beam file, produce one json file representing the Erlang AST.",
            "",
            "You can specify beam files in several ways:",
            "   '--beam <PATH>' specifies the path to one beam file",
            "   '--directory <DIR>' specifies all the beam files in <DIR>",
            "   '--list <FILE>' specifies all the paths listed, one per line, in <FILE>",
            "   '--otp' specifies all the beam files in OTP",
            "",
            "For the beam files coming after '--specs-only',",
            "the AST is filtered to keep only specs."
        ],
    lists:foreach(fun(U) -> io:fwrite("~s~n", [U]) end, Usage),
    halt(1).

main(Args) ->
    do_main(Args, false, {[], []}, []).

do_main(["--specs-only" | Rest], _SpecsOnly, Beams, Args) ->
    do_main(Rest, true, Beams, Args);
do_main(["--beam", BeamPath | Rest], SpecsOnly, Beams, Args) ->
    do_main(Rest, SpecsOnly, add_beams(SpecsOnly, [BeamPath], Beams), Args);
do_main(["--directory", Dir | Rest], SpecsOnly, Beams, Args) ->
    do_main(Rest, SpecsOnly, add_beams(SpecsOnly, beams_from_dir(Dir), Beams), Args);
do_main(["--list", File | Rest], SpecsOnly, Beams, Args) ->
    do_main(Rest, SpecsOnly, add_beams(SpecsOnly, beams_from_file(File), Beams), Args);
do_main(["--otp" | Rest], SpecsOnly, Beams, Args) ->
    do_main(Rest, SpecsOnly, add_beams(SpecsOnly, beams_from_otp(), Beams), Args);
do_main([Arg | Rest], SpecsOnly, Beams, []) ->
    do_main(Rest, SpecsOnly, Beams, [Arg]);
do_main([], _SpecsOnly, {FullBeams, SpecsOnlyBeams}, [OutDir]) ->
    OTPListFilePath = filename:join(OutDir, ?OTP_MODULES_LIST),
    filelib:ensure_dir(OTPListFilePath),
    OTPModulesList = lists:map(
        fun(Path) -> filename:basename(Path, ".beam") ++ "\n" end, beams_from_otp()
    ),
    ok = file:write_file(OTPListFilePath, io_lib:fwrite(OTPModulesList, [])),
    gather(mapper(OutDir, SpecsOnlyBeams, true)),
    gather(mapper(OutDir, FullBeams, false));
do_main(_, _, _, _) ->
    usage().

add_beams(true, NewBeams, {FullBeams, SpecsOnlyBeams}) -> {FullBeams, NewBeams ++ SpecsOnlyBeams};
add_beams(false, NewBeams, {FullBeams, SpecsOnlyBeams}) -> {NewBeams ++ FullBeams, SpecsOnlyBeams}.

beams_from_dir(Dir) ->
    filelib:wildcard(filename:join(Dir, "*.beam")).
beams_from_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            string:tokens(binary_to_list(Bin), "\n");
        E ->
            io:format("error reading: ~s: ~p~n", [File, E]),
            halt(1)
    end.

beams_from_otp() ->
    Glob = filename:join([code:lib_dir(), "*", "ebin", "*.beam"]),
    filelib:wildcard(Glob).

mapper(OutDir, Beams, SpecsOnly) ->
    Handler = fun(B) -> fun() -> exit(process_beam(B, OutDir, SpecsOnly)) end end,
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

filter_specs([H | T], Acc) ->
    case H of
        {attribute, _, spec, _} -> filter_specs(T, [H | Acc]);
        {attribute, _, type, _} -> filter_specs(T, [H | Acc]);
        {attribute, _, file, _} -> filter_specs(T, [H | Acc]);
        {attribute, _, module, _} -> filter_specs(T, [H | Acc]);
        {attribute, _, record, _} -> filter_specs(T, [H | Acc]);
        _ -> filter_specs(T, Acc)
    end;
filter_specs([], Acc) ->
    lists:reverse(Acc).

%% this runs in its own process
process_beam(BeamFilePath, OutDir, SpecsOnly) ->
    case get_ast(BeamFilePath) of
        {ok, Module, Forms} ->
            Forms1 =
                case SpecsOnly of
                    true -> filter_specs(Forms, []);
                    _ -> Forms
                end,
            JSON = ast_to_json(Forms1),
            OutFilePath = filename:join(OutDir, Module ++ ".json"),
            ok = file:write_file(OutFilePath, JSON);
        {error, E} ->
            exit(#{BeamFilePath => E})
    end.

get_ast(BeamFilePath) ->
    case beam_lib:chunks(BeamFilePath, [abstract_code]) of
        {ok, {Module, [{abstract_code, {_, Forms}}]}} ->
            {ok, atom_to_list(Module), Forms};
        {ok, {_, [{abstract_code, no_abstract_code}]}} ->
            % should be compiled with +debug_info
            {error, no_abstract_code};
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
