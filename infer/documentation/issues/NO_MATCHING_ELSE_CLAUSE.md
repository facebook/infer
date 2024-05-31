No matching else clause in Erlang: Reports an error when none of the clauses of an `else` match the short-circuit result from `maybe` body. Corresponds to the `{else_clause,V}` error in the Erlang runtime.

For example, here the `1 ?= 2` expression does not match and short-circuits to `2`, which does not match the single clause under `else`:
```erlang
else_clause_error() ->
    maybe
        1 ?= 2
    else
        1 -> ok
    end.
```

This error is reported if either the pattern(s) or the guard(s) prevent matching any of the clauses.
