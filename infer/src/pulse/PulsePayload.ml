(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include SummaryPayload.Make (struct
  type nonrec t = PulseSummary.t

  let field = Payloads.Fields.pulse
end)
