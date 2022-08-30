(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseModelsImport

module ProcNameDispatcher = struct
  let dispatch : (Tenv.t * Procname.t, model, arg_payload) ProcnameDispatcher.Call.dispatcher =
    ProcnameDispatcher.Call.make_dispatcher
      ( FbPulseModels.matchers @ PulseModelsCSharp.matchers
      @ PulseModelsObjC.transfer_ownership_matchers @ PulseModelsCpp.abort_matchers
      @ PulseModelsAndroid.matchers @ PulseModelsC.matchers @ PulseModelsCpp.matchers
      @ PulseModelsErlang.matchers @ PulseModelsGenericArrayBackedCollection.matchers
      @ PulseModelsJava.matchers @ PulseModelsObjC.matchers @ PulseModelsOptional.matchers
      @ PulseModelsSmartPointers.matchers @ Basic.matchers )
end

let dispatch tenv proc_name args = ProcNameDispatcher.dispatch (tenv, proc_name) proc_name args
