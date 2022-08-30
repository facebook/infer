Bad key in Erlang: Reports an error when trying to access or update a non-existing key in a map. Corresponds to the `{badkey,K}` error in the Erlang runtime.

For example, trying to update the key `2` in `M` gives `{badkey,2}` error because `2` is not present as a key in `M`.
```erlang
f() ->
    M = #{},
    M#{2 := 3}.
```

Note that maps currently use a recency abstraction, meaning that only the most recent key/value is tracked.
Therefore, if a map is non-empty and we try to access a key other than the one we track, we just assume that it is there to avoid false positives.
