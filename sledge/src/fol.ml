(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Var = Ses.Term.Var

module Term = struct
  include Ses.Term

  type term = t
  type formula = t

  let ite = conditional
end

module Context = Ses.Equality
