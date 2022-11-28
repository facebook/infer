(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

[@@@warning "-unused-value-declaration"]

module Let_syntax : sig
  include module type of Result.Monad_infix

  val ( let+ ) : ('ok, 'err) result -> ('ok -> 'okk) -> ('okk, 'err) result

  val ( let* ) : ('ok, 'err) result -> ('ok -> ('okk, 'err) result) -> ('okk, 'err) result
end
