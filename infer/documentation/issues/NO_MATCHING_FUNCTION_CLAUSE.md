No matching function clause in Erlang: Reports an error when none of the clauses of a function match the arguments of a call. Corresponds to the `function_clause` error in the Erlang runtime.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail([_|Xs]) -> Xs.
```

This error is reported if either the pattern(s) or the guard(s) prevent matching any of the clauses.
