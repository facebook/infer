% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(erl_hi_app).

-export([main/0]).

odd_matcher([_, _ | T]) ->
    odd_matcher(T);
odd_matcher([_]) ->
    ok.

maybe_good_matcher([_]) ->
    ok;
maybe_good_matcher(Xs) ->
    case length(Xs) rem 2 of
        0 -> maybe_good_matcher(half_it(Xs));
        1 -> maybe_good_matcher(Xs ++ Xs ++ Xs ++ [rabbit])
    end.

half_it([]) ->
    [];
half_it([_, X | T]) ->
    [X | half_it(T)].

guarded(Xs) ->
    if
        length(Xs) rem 2 == 1 -> odd_matcher(Xs);
        true -> ok
    end.

main() ->
    % ok, FP, nothing after that is explored
    maybe_good_matcher("odd"),
    % ok
    maybe_good_matcher("even"),
    % ok
    guarded("odd"),
    % ok
    guarded("even"),
    % ok
    odd_matcher("odd"),
    % nok
    odd_matcher("even"),
    % nok (not a good matcher after all)
    maybe_good_matcher("").
