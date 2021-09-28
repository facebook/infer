No true branch when evaluating an if expression in Erlang: Reports an error when none of the branches of an `if` expression evaluate to true. Corresponds to the `if_clause` error in the Erlang runtime.

For example, if we call `sign(0)` and the full definition of `sign` is
```erlang
sign(X) ->
    if
        X > 0 -> positive;
        X < 0 -> negative
    end.
```
