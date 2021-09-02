#!/usr/bin/env escript
% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

usage() ->
    Usage =
        "Usage:\n"
        "    extract.escript <compiled_list_path> <outdir>\n"
        "\n"
        "This script extracts in <outdir> the AST of all compiled\n"
        "BEAM files listed in <compiled_list_path>",
    io:format("~s", [Usage]),
    halt(1).

main([ListPath, OutDir]) ->
    Beams =
        case file:consult(ListPath) of
            {ok, Contents} ->
                Contents;
            Error ->
                io:format("error while reading file `~s`: ~p~n", [ListPath, Error]),
                halt(1)
        end,
    Parent = self(),
    gather([
        spawn(
            fun() -> Parent ! {self(), catch process_beam(Beam, OutDir)} end
        )
     || Beam <- Beams
    ]);
main(_) ->
    usage().

gather([H | T]) ->
    receive
        {H, _} -> gather(T)
    end;
gather([]) ->
    ok.

process_beam(BeamFilePath, OutDir) ->
    case get_ast(BeamFilePath) of
        {ok, Module, Forms} ->
            OutFilePath = filename:join(OutDir, atom_to_list(Module) ++ ".json"),
            dump_ast_as_json(Forms, OutFilePath);
        {error, What} ->
            io:format("error while getting AST from `~s`: ~p~n", [BeamFilePath, What])
    end.

get_ast(BeamFilePath) ->
    case beam_lib:chunks(BeamFilePath, [abstract_code]) of
        {ok, {Module, [{abstract_code, {_, Forms}}]}} ->
            {ok, Module, Forms};
        {error, _, What} ->
            {error, What}
    end.

dump_ast_as_json(Forms, OutFilePath) ->
    Object = ast_to_json(Forms),
    Contents = jsone:encode(Object),
    file:write_file(OutFilePath, Contents).

ast_to_json([]) ->
    [];
ast_to_json(Node) when is_list(Node) ->
    case lists:all(fun(Item) -> is_integer(Item) end, Node) of
        true -> unicode:characters_to_binary(Node);
        false -> [ast_to_json(Child) || Child <- Node]
    end;
ast_to_json(Node) when is_tuple(Node) ->
    L = tuple_to_list(Node),
    case lists:all(fun(Item) -> is_integer(Item) end, L) of
        true -> [ast_to_json(Child) || Child <- L];
        false -> ast_to_json(L)
    end;
ast_to_json(Node) ->
    Node.
