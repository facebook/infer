Bad return in Erlang: The dynamic type of a returned value disagrees with the static type given in the spec.

For example, this function returns an integer, while the spec says it returns an atom.
```erlang
-spec f() -> atom().
f() -> 1.
```

Note that this will *not* lead to a runtime error when running the Erlang program.
