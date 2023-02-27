% Copyright (c) Facebook, Inc. and its affiliates.
%
% This source code is licensed under the MIT license found in the
% LICENSE file in the root directory of this source tree.

-module(topl_taint_genserver).
-behaviour(gen_server).

% Both the client and the server are put in one file for simplicity

% Server API
-export([
    start_link/0,
    do_something/2,
    make_sanitizer/1,
    make_propagator/1,
    close_server/1
]).

% gen_server behaviour
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

% Client exports for testing
-export([
    fn_test_genserver1_Bad/0,
    test_genserver2_Ok/0,
    fn_test_genserver3_Bad/0,
    test_genserver4_Ok/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server side
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

% Get a message and reply based on the state: propagate or sanitize
do_something(Pid, Value) ->
    gen_server:call(Pid, {reply_to_message, Value}).

% Set the state to sanitize
make_sanitizer(Pid) ->
    gen_server:cast(Pid, {change_state, sanitize}).

% Set the state to propagate
make_propagator(Pid) ->
    gen_server:cast(Pid, {change_state, propagate}).

close_server(Pid) ->
    gen_server:stop(Pid).

%% gen_server functions

init([]) -> {ok, propagate}.

handle_call({reply_to_message, _Value}, _From, sanitize) ->
    {reply, non_dirty, sanitize};
handle_call({reply_to_message, Value}, _From, propagate) ->
    {reply, Value, propagate}.

handle_cast({change_state, Value}, _State) ->
    {noreply, Value}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client side
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fn_test_genserver1_Bad() ->
    case topl_taint_genserver:start_link() of
        {ok, Pid} ->
            Reply = topl_taint_genserver:do_something(Pid, source()),
            sink(Reply);
        % So that we don't complain about nonmatch
        _ ->
            ok
    end.

test_genserver2_Ok() ->
    case topl_taint_genserver:start_link() of
        {ok, Pid} ->
            _ = topl_taint_genserver:make_sanitizer(Pid),
            Reply = topl_taint_genserver:do_something(Pid, source()),
            sink(Reply);
        % So that we don't complain about nonmatch
        _ ->
            ok
    end.

fn_test_genserver3_Bad() ->
    case topl_taint_genserver:start_link() of
        {ok, Pid} ->
            _ = topl_taint_genserver:make_sanitizer(Pid),
            _ = topl_taint_genserver:make_propagator(Pid),
            Reply = topl_taint_genserver:do_something(Pid, source()),
            sink(Reply);
        % So that we don't complain about nonmatch
        _ ->
            ok
    end.

test_genserver4_Ok() ->
    case topl_taint_genserver:start_link() of
        {ok, Pid} ->
            _ = topl_taint_genserver:make_sanitizer(Pid),
            _ = topl_taint_genserver:make_propagator(Pid),
            _ = topl_taint_genserver:make_sanitizer(Pid),
            Reply = topl_taint_genserver:do_something(Pid, source()),
            sink(Reply);
        % So that we don't complain about nonmatch
        _ ->
            ok
    end.

%% Client helper functions

source() -> dirty.
% This should be something that crashes runtime (for our compiler tests),
% but is not reported by Pulse (so that we get TOPL error in TOPL tests).
sink(dirty) -> erlang:error(taint_error);
sink(_) -> ok.
