% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(infer_parse_transform).

-export([parse_transform/2]).

-type forms() :: erl_parse:abstract_form() | erl_parse:form_info().

-spec parse_transform([forms()], [compile:option()]) -> [forms()].
parse_transform(Forms, Options) ->
    OutDir =
        case lists:keyfind(ast_outdir, 1, Options) of
            {ast_outdir, Dir} ->
                Dir;
            _ ->
                case lists:keyfind(outdir, 1, Options) of
                    {outdir, Dir} -> Dir;
                    _ -> error(abort)
                end
        end,
    FileName =
        case lists:keyfind(file, 3, Forms) of
            {attribute, _, file, {FilePath, _}} -> filename:basename(FilePath);
            _ -> error(abort)
        end,
    dump_ast_as_json(OutDir, FileName, Forms),
    Forms.

dump_ast_as_json(Dir, SourceFileName, Forms) ->
    FileName = filename:rootname(SourceFileName) ++ ".json",
    FilePath = filename:join(Dir, FileName),
    Object = ast_to_json(Forms),
    Contents = jsone:encode(Object),
    file:write_file(FilePath, Contents).

ast_to_json([]) ->
    [];
ast_to_json(Node) when is_list(Node) ->
    case lists:all(fun(Item) -> is_integer(Item) end, Node) of
        true -> unicode:characters_to_binary(Node);
        false -> [ast_to_json(Child) || Child <- Node]
    end;
ast_to_json(Node) when is_tuple(Node) ->
    L = tuple_to_list(Node),
    ast_to_json(L);
ast_to_json(Node) ->
    Node.
