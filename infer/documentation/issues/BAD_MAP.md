Bad map in Erlang: Reports an error when trying to access or update a key for a term that is not a map. Corresponds to the `{badmap,...}` error in the Erlang runtime.

For example, trying to update `L` as if it was a map gives `{badmap,[1,2,3]}` error because `L` is actually a list (`[1,2,3]`).
```erlang
f() ->
    L = [1,2,3],
    L#{1 => 2}.
```
