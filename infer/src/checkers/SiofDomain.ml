(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module VarNames = AbstractDomain.FiniteSet (String)
module BottomSiofTrace = AbstractDomain.BottomLifted (SiofTrace)
module Summary = AbstractDomain.Pair (BottomSiofTrace) (VarNames)
include Summary
