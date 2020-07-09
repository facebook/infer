(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Var = Ses.Term.Var

module Term = struct
  include Ses.Term

  let ite = conditional
end

module Formula = struct
  include Ses.Term

  let inject b = b
  let project e = Some e

  let of_exp e =
    let b = Term.of_exp e in
    match project b with Some p -> p | None -> dq Term.zero b
end

module Context = struct
  include Ses.Equality

  let and_formula = and_term
  let normalizef = normalize

  module Subst = struct
    include Subst

    let substf = subst
  end
end
