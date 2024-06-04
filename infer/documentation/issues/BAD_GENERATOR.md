Bad generator in Erlang: Reports an error when a wrong type is used in a generator. Corresponds to the `bad_generator` error in the Erlang runtime.

For example:
```erlang
list_instead_of_map() ->
    M = [],
    [{K, V} || K := V <- M]
```
