(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module T = struct
  type 'a t = ('a, Alarm.t) result
end

include Stdlib.Result

include Monad.Make (struct
  include T

  let return = Result.ok
  let bind = Result.bind
end)

let iter x ~f = iter f x
