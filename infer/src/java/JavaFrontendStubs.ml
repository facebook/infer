(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module JMain = struct
  let from_arguments ~sources:_ _ = ()

  let from_verbose_out _ = ()
end

module JSourceFileInfo = struct
  let debug_on_file _ = ()
end

module JSourceLocations = struct
  let debug_on_file _ = ()
end
