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
    fp_test_lyse_user_pattern_matching_fun_Ok/0,
    test_lyse_user_pattern_matching_fun_Bad/0,
    test_lyse_user_pattern_matching_fun_literals_Ok/0,
    test_lyse_user_pattern_matching_fun_literals_Bad/0,
    test_lyse_user_guard_Ok/0,
    test_lyse_user_guard_Bad/0
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
    case R#robot.type of
        industrial -> ok
    end.

test_lyse_robot_default_value_atom_Bad() ->
    R = #robot{},
    case R#robot.type of
        industrial -> warn(1)
    end.

test_lyse_robot_default_value_list_Ok() ->
    R = #robot{},
    case R#robot.details of
        [] -> ok
    end.

test_lyse_robot_default_value_list_Bad() ->
    R = #robot{},
    case R#robot.details of
        [] -> warn(1)
    end.

test_lyse_robot_default_value_undefined_Ok() ->
    R = #robot{},
    case R#robot.details of
        [] -> ok
    end.

test_lyse_robot_default_value_undefined_Bad() ->
    R = #robot{},
    case R#robot.details of
        [] -> warn(1)
    end.

test_lyse_robot_default_value_override_Ok() ->
    R = #robot{
        name = "Mechatron",
        type = handmade,
        details = ["Moved from the inside"]
    },
    case {R#robot.type, R#robot.hobbies} of
        {handmade, undefined} -> ok
    end.

test_lyse_robot_default_value_override_Bad() ->
    R = #robot{
        name = "Mechatron",
        type = handmade,
        details = ["Moved from the inside"]
    },
    case {R#robot.type, R#robot.hobbies} of
        {handmade, undefined} -> warn(1)
    end.

%% Probably related to string handling. See: T93361792
fp_test_lyse_robot_nested_Ok() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    case (NestedBot#robot.details)#robot.name of
        "erNest" -> ok
    end.

test_lyse_robot_nested_Bad() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    case (NestedBot#robot.details)#robot.name of
        "erNest" -> warn(1)
    end.

test_lyse_robot_nested_literals_Ok() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    case (NestedBot#robot.details)#robot.type of
        industrial -> ok
    end.

test_lyse_robot_nested_literals_Bad() ->
    NestedBot = #robot{details = #robot{name = "erNest"}},
    case (NestedBot#robot.details)#robot.type of
        industrial -> warn(1)
    end.

test_lyse_robot_field_number_Ok() ->
    case #robot.type of
        3 -> ok
    end.

test_lyse_robot_field_number_Bad() ->
    case #robot.type of
        3 -> warn(1)
    end.

%% Probably related to string handling. See: T93361792
fp_test_lyse_robot_update_Ok() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = ["Repaired by repairman" | Details]},
    case NewRob#robot.details of
        ["Repaired by repairman"] -> ok
    end.

test_lyse_robot_update_Bad() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = ["Repaired by repairman" | Details]},
    case NewRob#robot.details of
        ["Repaired by repairman"] -> warn(1)
    end.

test_lyse_robot_update_literals_Ok() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = [repaired | Details]},
    case NewRob#robot.details of
        [repaired] -> ok
    end.

test_lyse_robot_update_literals_Bad() ->
    Rob = #robot{name = "Ulbert", hobbies = ["trying to have feelings"]},
    Details = Rob#robot.details,
    NewRob = Rob#robot{details = [repaired | Details]},
    case NewRob#robot.details of
        [repaired] -> warn(1)
    end.

-record(user, {id = 0, name, group, age}).

%% Probably related to string handling. See: T93361792
fp_test_lyse_user_pattern_matching_fun_Ok() ->
    User = #user{name = "User", group = admin},
    F = fun
        (#user{name = Name, group = admin}) ->
            Name ++ " is allowed!";
        (#user{name = Name}) ->
            Name ++ " is not allowed"
    end,
    case F(User) of
        "User is allowed!" ->
            ok
    end.

test_lyse_user_pattern_matching_fun_Bad() ->
    User = #user{name = "User", group = admin},
    F = fun
        (#user{name = Name, group = admin}) ->
            Name ++ " is allowed!";
        (#user{name = Name}) ->
            Name ++ " is not allowed"
    end,
    case F(User) of
        "User is allowed!" ->
            warn(1)
    end.

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
    case F(User) of
        42 ->
            ok
    end.

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
    case F(User) of
        42 ->
            warn(1)
    end.

test_lyse_user_guard_Ok() ->
    User = #user{age = 16},
    F = fun
        (U = #user{}) when U#user.age > 18 ->
            allowed;
        (_) ->
            forbidden
    end,
    case F(User) of
        forbidden ->
            ok
    end.

test_lyse_user_guard_Bad() ->
    User = #user{age = 16},
    F = fun
        (U = #user{}) when U#user.age > 18 ->
            allowed;
        (_) ->
            forbidden
    end,
    case F(User) of
        forbidden ->
            warn(1)
    end.
