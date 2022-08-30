Bad record in Erlang: Reports an error when trying to access or update a record with the wrong name. Corresponds to the `{badrecord,Name}` error in the Erlang runtime.

For example, accessing `R` as a `person` record gives `{badrecord,person}` error because `R` is `rabbit` (even though both share the `name` field).
```erlang
-record(person, {name, phone}).
-record(rabbit, {name, color}).

f() ->
    R = #rabbit{name = "Bunny", color = "Brown"},
    R#person.name.
```
