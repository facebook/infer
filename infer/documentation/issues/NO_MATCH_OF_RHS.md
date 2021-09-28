No match of right hand side value in Erlang: Reports an error when the right hand side value of a `match` expression does not match the pattern on the left hand side. Corresponds to the `{badmatch,V}` error in the Erlang runtime.

For example, `[H|T] = []` gives the error because the left hand side pattern requires at least one element in the list on the right hand side.
