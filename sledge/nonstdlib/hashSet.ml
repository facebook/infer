(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include CCHashSet

module Make (E : ELEMENT) = struct
  include CCHashSet.Make [@inlined] (E)

  let update s e ~f =
    let eo = find s e in
    match (eo, f eo) with
    | None, None -> ()
    | Some e, Some e' when e == e' -> ()
    | Some _, None -> remove s e
    | _, Some e' -> insert s e'

  let add s e =
    let change = ref false in
    update s e ~f:(function
      | None ->
          change := true ;
          Some e
      | Some e -> Some e ) ;
    !change
end
