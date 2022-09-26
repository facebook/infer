(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(** data type for the control flow graph *)
type t = Procdesc.t Procname.Hash.t

let create () = Procname.Hash.create 16

let get_sorted_procs cfg =
  let compare_proc_desc_by_proc_name pdesc1 pdesc2 =
    Procname.compare (Procdesc.get_proc_name pdesc1) (Procdesc.get_proc_name pdesc2)
  in
  Procname.Hash.fold (fun _ pdesc acc -> pdesc :: acc) cfg []
  |> List.sort ~compare:compare_proc_desc_by_proc_name


let get_all_defined_proc_names cfg =
  let procs = ref [] in
  let f pname pdesc = if Procdesc.is_defined pdesc then procs := pname :: !procs in
  Procname.Hash.iter f cfg ;
  !procs


let create_proc_desc cfg (proc_attributes : ProcAttributes.t) =
  let pdesc = Procdesc.from_proc_attributes proc_attributes in
  let pname = proc_attributes.proc_name in
  if Procname.Hash.mem cfg pname then
    L.die InternalError "Creating two procdescs for the same procname." ;
  Procname.Hash.add cfg pname pdesc ;
  pdesc


let iter_sorted cfg ~f = get_sorted_procs cfg |> List.iter ~f

let fold_sorted cfg ~init ~f = get_sorted_procs cfg |> List.fold ~init ~f

let store source_file cfg =
  let save_proc _ proc_desc =
    let attributes = Procdesc.get_attributes proc_desc in
    let loc = attributes.loc in
    let attributes' =
      let loc' = if Location.equal loc Location.dummy then {loc with file= source_file} else loc in
      {attributes with loc= loc'; translation_unit= source_file}
    in
    Procdesc.set_attributes proc_desc attributes' ;
    Attributes.store
      ~proc_desc:(Option.some_if attributes.is_defined proc_desc)
      attributes' ~analysis:false
  in
  Procname.Hash.iter save_proc cfg


let pp_proc_signatures fmt cfg =
  F.fprintf fmt "@[<v>METHOD SIGNATURES@;" ;
  iter_sorted ~f:(Procdesc.pp_signature fmt) cfg ;
  F.fprintf fmt "@]"
