(*
* Copyright (c) 2014 - Facebook. All rights reserved.
*)

module ConstantMap: Map.S with type key = string

module ConstantFlow: Dataflow.DF with type state = (Sil.const option) ConstantMap.t

val run: Cfg.Procdesc.t -> (Cfg.Node.t -> ConstantFlow.state)
