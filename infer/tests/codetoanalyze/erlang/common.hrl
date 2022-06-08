% Throw a custom error so that when compiled and executed, we know what to expect.
% Infer doesn't support custom exceptions yet, so do a badmatch (1=2) in the end to
% make it report an error.
-define(EXPECTED_CRASH, begin
    erlang:error(expected_crash),
    1 = 2
end).
-define(UNEXPECTED_CRASH, begin
    erlang:error(unexpected_crash),
    1 = 2
end).

-define(CRASH_IF_EQUAL(Expected, Actual),
    case Actual of
        Expected -> ?EXPECTED_CRASH;
        _ -> ok
    end
).

-define(ASSERT_EQUAL(Expected, Actual),
    case Actual of
        Expected -> ok;
        _ -> ?UNEXPECTED_CRASH
    end
).
