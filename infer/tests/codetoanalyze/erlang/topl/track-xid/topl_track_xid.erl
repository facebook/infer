-module(topl_track_xid).
-export([direct/1, tuple/1, map/1]).

direct(X) ->
  b:src(X, "xid"),
  b:tgt(X, "xid").

tuple(X) ->
  {Y} = X,
  b:src(Y, "xid"),
  b:tgt(Y, "xid").

map(X) ->
  #{y:=Y} = X,
  b:src(Y, "xid"),
  b:tgt(Y, "xid").
