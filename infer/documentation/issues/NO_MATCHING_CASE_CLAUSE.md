No matching case clause in Erlang: Reports an error when none of the clauses of a `case` match the expression. Corresponds to the `{case_clause,V}` error in the Erlang runtime.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail(X) ->
    case X of
        [_|T] -> T
    end.
```

This error is reported if either the pattern(s) or the guard(s) prevent matching any of the clauses.
