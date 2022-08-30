Bad arg in Erlang: Reports an error when the type of an argument is wrong or the argument is badly formed. Corresponds to the `badarg` error in the Erlang runtime.

For example, trying to concatenate the number `3` with  the list `[1,2]` gives `badarg` error because `3` is not a list.
```erlang
f() ->
    3 ++ [1,2]. // badarg error
```

Note that although the first argument needs to be a list, the second argument may not be a list.
For instance, concatenating [1,2] with the number `3` raises no error in Erlang.
```erlang
g() ->
    [1,2] ++ 3. // no error. Result: [1,2|3]
```
