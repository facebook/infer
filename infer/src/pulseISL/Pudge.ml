(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Pudge = ( val if Config.pudge then (module PulseISLSledge) else (module PulseISLDummySledge)
                   : Pudge_intf.S )

include Pudge
