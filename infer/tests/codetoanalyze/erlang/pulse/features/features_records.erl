% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_records).

-record(person, {name, phone, address}).
-record(car, {plate, owner}).

-export([
    test_index2_Ok/0,
    test_index2_Bad/0,
    test_index3_Ok/0,
    test_index3_Bad/0,
    test_index4_Ok/0,
    test_index4_Bad/0,
    test_field2_Ok/0,
    test_field2_Bad/0,
    test_field3_Ok/0,
    test_field3_Bad/0,
    test_field4_Ok/0,
    test_field4_Bad/0,
    test_field_rearranged_Ok/0,
    test_field_rearranged_Bad/0,
    test_field_all_other1_Ok/0,
    test_field_all_other1_Bad/0,
    test_field_all_other2_Ok/0,
    test_field_all_other2_Bad/0,
    test_field_all_other3_Ok/0,
    test_field_all_other3_Bad/0,
    test_field_update1_Ok/0,
    test_field_update1_Bad/0,
    test_field_update2_Ok/0,
    test_field_update2_Bad/0,
    test_field_update3_Ok/0,
    test_field_update3_Bad/0,
    test_initializer1_Ok/0,
    test_initializer1_Bad/0,
    test_initializer2_Ok/0,
    test_initializer2_Bad/0,
    test_initializer_explicit_override_Ok/0,
    test_initializer_explicit_override_Bad/0,
    test_initializer_update_override_Ok/0,
    test_initializer_update_override_Bad/0,
    test_undefined_Ok/0,
    test_undefined_Bad/0,
    test_nested_Ok/0,
    test_nested_Bad/0
]).

% Call this method with warn(1) to trigger a warning to expect
warn(0) -> ok.

test_index2_Ok() ->
    case #person.name of
        2 -> ok
    end.

test_index2_Bad() ->
    case #person.name of
        2 -> warn(1)
    end.

test_index3_Ok() ->
    case #person.phone of
        3 -> ok
    end.

test_index3_Bad() ->
    case #person.phone of
        3 -> warn(1)
    end.

test_index4_Ok() ->
    case #person.address of
        4 -> ok
    end.

test_index4_Bad() ->
    case #person.address of
        4 -> warn(1)
    end.

test_field2_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    case P#person.name of
        123 -> ok
    end.

test_field2_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    case P#person.name of
        123 -> warn(1)
    end.

test_field3_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    case P#person.phone of
        45 -> ok
    end.

test_field3_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    case P#person.phone of
        45 -> warn(1)
    end.

test_field4_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    case P#person.address of
        6789 -> ok
    end.

test_field4_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    case P#person.address of
        6789 -> warn(1)
    end.

test_field_rearranged_Ok() ->
    % Fields are set in different order
    P = #person{phone = 45, address = 6789, name = 123},
    case P#person.name of
        123 -> ok
    end.

test_field_rearranged_Bad() ->
    % Fields are set in different order
    P = #person{phone = 45, address = 6789, name = 123},
    case P#person.name of
        123 -> warn(1)
    end.

test_field_all_other1_Ok() ->
    P = #person{phone = 45, _ = 123},
    case P#person.name of
        123 -> ok
    end.

test_field_all_other1_Bad() ->
    P = #person{phone = 45, _ = 123},
    case P#person.name of
        123 -> warn(1)
    end.

test_field_all_other2_Ok() ->
    P = #person{phone = 45, _ = 123},
    case P#person.address of
        123 -> ok
    end.

test_field_all_other2_Bad() ->
    P = #person{phone = 45, _ = 123},
    case P#person.address of
        123 -> warn(1)
    end.

test_field_all_other3_Ok() ->
    P = #person{phone = 45, _ = 123},
    case P#person.phone of
        45 -> ok
    end.

test_field_all_other3_Bad() ->
    P = #person{phone = 45, _ = 123},
    case P#person.phone of
        45 -> warn(1)
    end.

test_field_update1_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    case Q#person.phone of
        0 -> ok
    end.

test_field_update1_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    case Q#person.phone of
        0 -> warn(1)
    end.

test_field_update2_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    case Q#person.name of
        123 -> ok
    end.

test_field_update2_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    case Q#person.name of
        123 -> warn(1)
    end.

test_field_update3_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    case Q#person.address of
        6789 -> ok
    end.

test_field_update3_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    case Q#person.address of
        6789 -> warn(1)
    end.

-record(rabbit, {name = 123, color = 45}).

test_initializer1_Ok() ->
    R = #rabbit{},
    case R#rabbit.name of
        123 -> ok
    end.

test_initializer1_Bad() ->
    R = #rabbit{},
    case R#rabbit.name of
        123 -> warn(1)
    end.

test_initializer2_Ok() ->
    R = #rabbit{},
    case R#rabbit.color of
        45 -> ok
    end.

test_initializer2_Bad() ->
    R = #rabbit{},
    case R#rabbit.color of
        45 -> warn(1)
    end.

test_initializer_explicit_override_Ok() ->
    R = #rabbit{name = 6789},
    case R#rabbit.name of
        6789 -> ok
    end.

test_initializer_explicit_override_Bad() ->
    R = #rabbit{name = 6789},
    case R#rabbit.name of
        6789 -> warn(1)
    end.

test_initializer_update_override_Ok() ->
    R = #rabbit{name = 987, color = 65},
    Q = R#rabbit{name = 4321},
    case Q#rabbit.name of
        4321 -> ok
    end.

test_initializer_update_override_Bad() ->
    R = #rabbit{name = 987, color = 65},
    Q = R#rabbit{name = 4321},
    case Q#rabbit.name of
        4321 -> warn(1)
    end.

test_undefined_Ok() ->
    P = #person{},
    case P#person.name of
        undefined -> ok
    end.

test_undefined_Bad() ->
    P = #person{},
    case P#person.name of
        undefined -> warn(1)
    end.

test_nested_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    C = #car{plate = 987654, owner = P},
    case C#car.owner#person.name of
        123 -> ok
    end.

test_nested_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    C = #car{plate = 987654, owner = P},
    case C#car.owner#person.name of
        123 -> warn(1)
    end.
