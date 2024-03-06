% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(infer_parse_transform).

-export([parse_transform/2]).

-type forms() :: erl_parse:abstract_form() | erl_parse:form_info().

-spec parse_transform([forms()], [compile:option()]) -> [forms()].
parse_transform(Forms, Options) ->
    {infer_compiled_list_path, ListPath} = lists:keyfind(infer_compiled_list_path, 1, Options),
    FileName =
        case lists:keyfind(file, 3, Forms) of
            {attribute, _, file, {FilePath, _}} ->
                BaseName = filename:basename(FilePath),
                filename:rootname(BaseName)
        end,
    {outdir, BeamDir} = lists:keyfind(outdir, 1, Options),
    BeamFilePath = filename:join(BeamDir, FileName ++ ".beam"),
    {ok, Handle} = file:open(ListPath, [append]),
    io:format(Handle, "~s~n", [BeamFilePath]),
    file:close(Handle),
    Forms.
