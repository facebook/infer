% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(features_records).
-include("../../common.hrl").

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
    test_nested_Bad/0,
    %% Tests inspired from the "Learn You Some Erlang" (lyse) book
    %% https://learnyousomeerlang.com/a-short-visit-to-common-data-structures#records
    test_lyse_robot_default_value_atom_Ok/0,
    test_lyse_robot_default_value_atom_Bad/0,
    test_lyse_robot_default_value_list_Ok/0,
    test_lyse_robot_default_value_list_Bad/0,
    test_lyse_robot_default_value_undefined_Ok/0,
    test_lyse_robot_default_value_undefined_Bad/0,
    test_lyse_robot_default_value_override_Ok/0,
    test_lyse_robot_default_value_override_Bad/0,
    fp_test_lyse_robot_nested_Ok/0,
    test_lyse_robot_nested_Bad/0,
    test_lyse_robot_nested_literals_Ok/0,
    test_lyse_robot_nested_literals_Bad/0,
    test_lyse_robot_field_number_Ok/0,
    test_lyse_robot_field_number_Bad/0,
    fp_test_lyse_robot_update_Ok/0,
    test_lyse_robot_update_Bad/0,
    test_lyse_robot_update_literals_Ok/0,
    test_lyse_robot_update_literals_Bad/0,
    test_lyse_user_pattern_matching_fun_Ok/0,
    fn_test_lyse_user_pattern_matching_fun_Bad/0,
    test_lyse_user_pattern_matching_fun_literals_Ok/0,
    test_lyse_user_pattern_matching_fun_literals_Bad/0,
    test_lyse_user_guard_Ok/0,
    test_lyse_user_guard_Bad/0
]).

test_index2_Ok() ->
    ?ASSERT_EQUAL(2, #person.name).

test_index2_Bad() ->
    ?CRASH_IF_EQUAL(2, #person.name).

test_index3_Ok() ->
    ?ASSERT_EQUAL(3, #person.phone).

test_index3_Bad() ->
    ?CRASH_IF_EQUAL(3, #person.phone).

test_index4_Ok() ->
    ?ASSERT_EQUAL(4, #person.address).

test_index4_Bad() ->
    ?CRASH_IF_EQUAL(4, #person.address).

test_field2_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    ?ASSERT_EQUAL(123, P#person.name).

test_field2_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    ?CRASH_IF_EQUAL(123, P#person.name).

test_field3_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    ?ASSERT_EQUAL(45, P#person.phone).

test_field3_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    ?CRASH_IF_EQUAL(45, P#person.phone).

test_field4_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    ?ASSERT_EQUAL(6789, P#person.address).

test_field4_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    ?CRASH_IF_EQUAL(6789, P#person.address).

test_field_rearranged_Ok() ->
    % Fields are set in different order
    P = #person{phone = 45, address = 6789, name = 123},
    ?ASSERT_EQUAL(123, P#person.name).

test_field_rearranged_Bad() ->
    % Fields are set in different order
    P = #person{phone = 45, address = 6789, name = 123},
    ?CRASH_IF_EQUAL(123, P#person.name).

test_field_all_other1_Ok() ->
    P = #person{phone = 45, _ = 123},
    ?ASSERT_EQUAL(123, P#person.name).

test_field_all_other1_Bad() ->
    P = #person{phone = 45, _ = 123},
    ?CRASH_IF_EQUAL(123, P#person.name).

test_field_all_other2_Ok() ->
    P = #person{phone = 45, _ = 123},
    ?ASSERT_EQUAL(123, P#person.address).

test_field_all_other2_Bad() ->
    P = #person{phone = 45, _ = 123},
    ?CRASH_IF_EQUAL(123, P#person.address).

test_field_all_other3_Ok() ->
    P = #person{phone = 45, _ = 123},
    ?ASSERT_EQUAL(45, P#person.phone).

test_field_all_other3_Bad() ->
    P = #person{phone = 45, _ = 123},
    ?CRASH_IF_EQUAL(45, P#person.phone).

test_field_update1_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    ?ASSERT_EQUAL(0, Q#person.phone).

test_field_update1_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    ?CRASH_IF_EQUAL(0, Q#person.phone).

test_field_update2_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    ?ASSERT_EQUAL(123, P#person.name).

test_field_update2_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    ?CRASH_IF_EQUAL(123, P#person.name).

test_field_update3_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    ?ASSERT_EQUAL(6789, Q#person.address).

test_field_update3_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    Q = P#person{phone = 0},
    ?CRASH_IF_EQUAL(6789, Q#person.address).

-record(rabbit, {name = 123, color = 45}).

test_initializer1_Ok() ->
    R = #rabbit{},
    ?ASSERT_EQUAL(123, R#rabbit.name).

test_initializer1_Bad() ->
    R = #rabbit{},
    ?CRASH_IF_EQUAL(123, R#rabbit.name).

test_initializer2_Ok() ->
    R = #rabbit{},
    ?ASSERT_EQUAL(45, R#rabbit.color).

test_initializer2_Bad() ->
    R = #rabbit{},
    ?CRASH_IF_EQUAL(45, R#rabbit.color).

test_initializer_explicit_override_Ok() ->
    R = #rabbit{name = 6789},
    ?ASSERT_EQUAL(6789, R#rabbit.name).

test_initializer_explicit_override_Bad() ->
    R = #rabbit{name = 6789},
    ?CRASH_IF_EQUAL(6789, R#rabbit.name).

test_initializer_update_override_Ok() ->
    R = #rabbit{name = 987, color = 65},
    Q = R#rabbit{name = 4321},
    ?ASSERT_EQUAL(4321, Q#rabbit.name).

test_initializer_update_override_Bad() ->
    R = #rabbit{name = 987, color = 65},
    Q = R#rabbit{name = 4321},
    ?CRASH_IF_EQUAL(4321, Q#rabbit.name).

test_undefined_Ok() ->
    P = #person{},
    ?ASSERT_EQUAL(undefined, P#person.name).

test_undefined_Bad() ->
    P = #person{},
    ?CRASH_IF_EQUAL(undefined, P#person.name).

test_nested_Ok() ->
    P = #person{name = 123, phone = 45, address = 6789},
    C = #car{plate = 987654, owner = P},
    ?ASSERT_EQUAL(123, C#car.owner#person.name).

test_nested_Bad() ->
    P = #person{name = 123, phone = 45, address = 6789},
    C = #car{plate = 987654, owner = P},
    ?CRASH_IF_EQUAL(123, C#car.owner#person.name).

%% Tests inspired from the "Learn You Some Erlang" (lyse) book
%% https://learnyousomeerlang.com/a-short-visit-to-common-data-structures#records

-record(robot, {
    name,
    type = industrial,
    hobbies,
    details = []
}).

test_lyse_robot_default_value_atom_Ok() ->
    R = #robot{},
    ?ASSERT_EQUAL(industrial, R#robot.type).

test_lyse_robot_default_value_atom_Bad() ->
    R = #robot{},
    ?CRASH_IF_EQUAL(industrial, R#robot.type).

test_lyse_robot_default_value_list_Ok() ->
    R = #robot{},
    ?ASSERT_EQUAL([], R#robot.details).

test_lyse_robot_default_value_list_Bad() ->
    R = #robot{},
    ?CRASH_IF_EQUAL([], R#robot.details).

test_lyse_robot_default_value_undefined_Ok() ->
    R = #robot{},
    ?ASSERT_EQUAL(undefined, R#robot.name).

test_lyse_robot_default_value_undefined_Bad() ->
    R = #robot{},
    ?CRASH_IF_EQUAL(undefined, R#robot.name).

test_lyse_robot_default_value_override_Ok() ->
    R = #robot{
        name = "Mechatron",
        type = handmade,
        details = ["Moved from the inside"]
    },
    ?ASSERT_EQUAL({handmade, undefined}, {R#robot.type, R#robot.hobbies}).

test_lyse_robot_default_value_override_Bad() ->
    R = #robot{
        name = "Mechatron",
        type = handmade,
        details = ["Moved from the inside"]
    },
    ?CRASH_IF_EQUAL({handmade, undefined}, {R#robot.type, R#robot.hobbies}).

%% Probably related to string handling. See: T93361792
fp_test_lyse_robot_nested_Ok() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    ?ASSERT_EQUAL("erNest", (NestedBot#robot.details)#robot.name).

test_lyse_robot_nested_Bad() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    ?CRASH_IF_EQUAL("erNest", (NestedBot#robot.details)#robot.name).

test_lyse_robot_nested_literals_Ok() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    ?ASSERT_EQUAL(industrial, (NestedBot#robot.details)#robot.type).

test_lyse_robot_nested_literals_Bad() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    ?CRASH_IF_EQUAL(industrial, (NestedBot#robot.details)#robot.type).

test_lyse_robot_field_number_Ok() ->
    ?ASSERT_EQUAL(3, #robot.type).

test_lyse_robot_field_number_Bad() ->
    ?CRASH_IF_EQUAL(3, #robot.type).

%% Probably related to string handling. See: T93361792
fp_test_lyse_robot_update_Ok() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = ["Repaired by repairman" | Details]},
    ?ASSERT_EQUAL(["Repaired by repairman"], NewRob#robot.details).

test_lyse_robot_update_Bad() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = ["Repaired by repairman" | Details]},
    ?CRASH_IF_EQUAL(["Repaired by repairman"], NewRob#robot.details).

test_lyse_robot_update_literals_Ok() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = [repaired | Details]},
    ?ASSERT_EQUAL([repaired], NewRob#robot.details).

test_lyse_robot_update_literals_Bad() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = [repaired | Details]},
    ?CRASH_IF_EQUAL([repaired], NewRob#robot.details).

-record(user, {id = 0, name, group, age}).

test_lyse_user_pattern_matching_fun_Ok() ->
    User = #user{name = "User", group = admin},
    F = fun
        (#user{name = Name, group = admin}) ->
            Name ++ " is allowed!";
        (#user{name = Name}) ->
            Name ++ " is not allowed"
    end,
    ?ASSERT_EQUAL("User is allowed!", F(User)).

fn_test_lyse_user_pattern_matching_fun_Bad() ->
    User = #user{name = "User", group = admin},
    F = fun
        (#user{name = Name, group = admin}) ->
            Name ++ " is allowed!";
        (#user{name = Name}) ->
            Name ++ " is not allowed"
    end,
    ?CRASH_IF_EQUAL("User is allowed!", F(User)).

test_lyse_user_pattern_matching_fun_literals_Ok() ->
    User = #user{name = "User", group = admin},
    F = fun
        (#user{id = Id, group = admin}) ->
            Id + 42;
        (#user{id = Id}) ->
            Id;
        (_) ->
            42
    end,
    ?ASSERT_EQUAL(42, F(User)).

test_lyse_user_pattern_matching_fun_literals_Bad() ->
    User = #user{name = "User", group = admin},
    F = fun
        (#user{id = Id, group = admin}) ->
            Id + 42;
        (#user{id = Id}) ->
            Id;
        (_) ->
            42
    end,
    ?CRASH_IF_EQUAL(42, F(User)).

test_lyse_user_guard_Ok() ->
    User = #user{age = 16},
    F = fun
        (U = #user{}) when U#user.age > 18 ->
            allowed;
        (_) ->
            forbidden
    end,
    ?ASSERT_EQUAL(forbidden, F(User)).

test_lyse_user_guard_Bad() ->
    User = #user{age = 16},
    F = fun
        (U = #user{}) when U#user.age > 18 ->
            allowed;
        (_) ->
            forbidden
    end,
    ?CRASH_IF_EQUAL(forbidden, F(User)).
