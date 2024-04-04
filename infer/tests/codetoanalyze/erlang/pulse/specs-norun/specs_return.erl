% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(specs_return).

-export([
    test_simple1_Bad/0,
    test_simple2_Bad/0,
    test_simple3_Ok/0,
    test_simple4_Ok/0,
    test_simple5_Ok/0,
    test_userdefined1_Ok/0,
    test_userdefined2_Ok/0,
    test_userdefined3_Bad/0,
    test_userdefined4_Bad/0,
    test_userdefined5_Ok/1,
    fnl_test_userdefined6_Bad/1
]).

-type my_atom_type() :: atom().

-type my_union_type() :: my_atom_type() | map().

-spec test_simple1_Bad() -> atom().
test_simple1_Bad() -> 1.

-spec test_simple2_Bad() -> integer().
test_simple2_Bad() -> ok.

-spec test_simple3_Ok() -> boolean().
test_simple3_Ok() -> true.

-spec test_simple4_Ok() -> integer().
test_simple4_Ok() -> 13.

-spec test_simple5_Ok() -> atom().
test_simple5_Ok() -> ok.

-spec test_userdefined1_Ok() -> my_atom_type().
test_userdefined1_Ok() -> ok.

-spec test_userdefined2_Ok() -> my_union_type().
test_userdefined2_Ok() -> ok.

-spec test_userdefined3_Bad() -> my_atom_type().
test_userdefined3_Bad() -> 1.

-spec test_userdefined4_Bad() -> my_union_type().
test_userdefined4_Bad() -> 1.

-spec test_userdefined5_Ok(atom()) -> my_union_type().
test_userdefined5_Ok(X) -> X.

-spec fnl_test_userdefined6_Bad(integer()) -> my_union_type().
fnl_test_userdefined6_Bad(X) -> X.
