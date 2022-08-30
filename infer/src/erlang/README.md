# Trying Infer on Simple Erlang Code

Before starting, please [install Infer from source](https://github.com/facebook/infer/blob/main/INSTALL.md#install-infer-from-source).
When following those instructions, build using the command `./build-infer.sh erlang`.
Also, make sure that the Erlang compiler `erlc` is installed.

## Reliability Issues

To see the error "no match of rhs" put the following in a file `ex1.erl`:


    -module(ex1).
    -export([bad/0, good/0]).
    bad() ->
        [H | _] = get_list(0),
        H.
    good() ->
        [H | _] = get_list(2),
        H.
    get_list(X) when X =< 0 -> [];
    get_list(X) when X > 0 -> [X | get_list(X - 1)].


Then run Infer with the following command:

    infer --pulse-only -- erlc ex1.erl

## User-Specified Properties

### Writing to a Closed File

Put the following in a file called `WriteAfterClose.topl`:

    property WriteAfterClose
      start -> start: *
      start -> closed: "file:close/1"(A,Ret) => f:=A
      closed -> error: "file:write/2"(F,D,Ret) when F==f

Put the following in a file called `ex2.erl`:

    -module(ex2).
    -export([bad/1,good/1]).
    good(F) -> nop(F), file:write(F, "hi").
    bad(F)  -> op(F), file:write(F, "hi").
    nop(_)  -> ok.
    op(F)   -> file:close(F).

Run the following command:

    infer --topl-only --topl-properties WriteAfterClose.topl -- erlc ex2.erl


### Taint

### Taint with Transformations

