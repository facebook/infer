(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

module Make (Spec : TraceElem.S) = struct
  include Trace.Make(struct
      module Source = Source.Dummy
      module Sink = struct
        include Spec
        let get _ _ = []
      end

      let should_report _ _ = true
    end)
end
