Bad key in Erlang: Reports an error when trying to access or update a non-existing key in a map. Corresponds to the `{badkey,K}` error in the Erlang runtime.

For example, trying to update the key `2` in `M` gives `{badkey,2}` error because `2` is not present as a key in `M`.
```erlang
f() ->
    M = #{1 => 2},
    M#{2 := 3}.
```
