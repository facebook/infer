No matching branch is found when evaluating the `of` section of a `try` expression. Corresponds to the `{try_clause,V}` error in the Erlang runtime.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail(X) ->
    try X of
        [_|T] -> {ok,T}
    catch
        _ -> error
    end.
```
