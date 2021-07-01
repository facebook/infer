## Nonexhaustive pattern match in Erlang

Reports an error when a function clause is missing.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail([_|Xs]) -> Xs.
```

The error is also reported if the failing pattern match is in a `case` expression.
