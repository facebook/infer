% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(nonmatch_tuples).

-export([
    test_empty3_Bad/0,
    test_empty2_Bad/0,
    test_empty1_Ok/0,
    test_size1_Ok/0,
    test_size2_Bad/0,
    test_size3_Bad/0,
    test_size4_Bad/0,
    test_elements1_Ok/0,
    test_elements2_Bad/0,
    test_elements3_Bad/0,
    test_elements_partial1_Ok/0,
    test_elements_partial2_Ok/0,
    test_elements_partial3_Bad/0,
    test_elements_partial4_Bad/0,
    test_nested1_Ok/0,
    test_nested2_Ok/0,
    test_nested3_Bad/0,
    test_nested4_Bad/0,
    test_first1_Ok/0,
    test_first2_Ok/0,
    test_first3_Ok/0,
    test_first4_Bad/0,
    test_first5_Bad/0
]).

accepts_tuple_of_two({_, _}) -> ok.

test_size1_Ok() ->
    accepts_tuple_of_two({1, 2}).

test_size2_Bad() ->
    accepts_tuple_of_two({1, 2, 3}).

test_size3_Bad() ->
    accepts_tuple_of_two({1}).

test_size4_Bad() ->
    accepts_tuple_of_two({}).

test_elements1_Ok() ->
    T = {1, 2},
    {1, 2} = T,
    ok.

test_elements2_Bad() ->
    T = {1, 2},
    {1, 3} = T,
    ok.

test_elements3_Bad() ->
    T = {1, 2},
    {1, 2, 3} = T,
    ok.

test_elements_partial1_Ok() ->
    T = {1, 2},
    {1, _} = T,
    ok.

test_elements_partial2_Ok() ->
    T = {1, 2},
    {_, 2} = T,
    ok.

test_elements_partial3_Bad() ->
    T = {1, 2},
    {2, _} = T,
    ok.

test_elements_partial4_Bad() ->
    T = {1, 2},
    {_, 1} = T,
    ok.

test_nested1_Ok() ->
    T = {1, {1, 2}, 3},
    {_, {_, 2}, 3} = T.

test_nested2_Ok() ->
    T = {{1, 2, 3}, {1, 2}, 3},
    {_, {_, 2}, 3} = T.

test_nested3_Bad() ->
    T = {1, {1, 2, 3}, 3},
    {_, {_, 2}, 3} = T.

test_nested4_Bad() ->
    T = {1, {1, 2}, 5},
    {_, {_, 2}, 3} = T.

first_from_at_most_three({X}) -> X;
first_from_at_most_three({X, _}) -> X;
first_from_at_most_three({X, _, _}) -> X.

test_first1_Ok() ->
    first_from_at_most_three({1}).

test_first2_Ok() ->
    first_from_at_most_three({1, 2}).

test_first3_Ok() ->
    first_from_at_most_three({1, 2, 3}).

test_first4_Bad() ->
    first_from_at_most_three({}).

test_first5_Bad() ->
    first_from_at_most_three({1, 2, 3, 4}).

accepts_empty({}) -> ok.

test_empty1_Ok() ->
    accepts_empty({}).

test_empty2_Bad() ->
    accepts_empty({1}).

test_empty3_Bad() ->
    accepts_empty({1, 2}).
