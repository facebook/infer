(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CallSites = AbstractDomain.FiniteSet (CallSite)
module SinkMap = AbstractDomain.Map (Typ.Procname) (CallSites)
include AbstractDomain.Map (Annot) (SinkMap)
