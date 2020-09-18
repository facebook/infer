(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Call = struct
  let dispatch : (Tenv.t, unit, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ +PatternMatch.ObjectiveC.implements "NSObject" &:: "autorelease" &--> ()
      ; -"CFAutorelease" &--> ()
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "initForReadingFromData:error:" &--> ()
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "initForReadingWithData:" &--> ()
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "unarchivedObjectOfClass:fromData:error:" &--> ()
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "unarchivedObjectOfClasses:fromData:error:" &--> () ]
end
