(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PulseBasicInterface

(** Stacks: map addresses of variables to values and histoy. *)

module VarAddress = struct
  include Var

  (* HACK: Ignore the global variable kinds in the comparison. *)
  let compare x y =
    match (x, y) with
    | ProgramVar x, ProgramVar y when Pvar.is_global x && Pvar.is_global y ->
        [%compare: Mangled.t * Typ.template_spec_info]
          (Pvar.get_name x, Pvar.get_template_args x)
          (Pvar.get_name y, Pvar.get_template_args y)
    | ProgramVar x, _ when Pvar.is_global x ->
        -1
    | _, ProgramVar x when Pvar.is_global x ->
        1
    | _, _ ->
        compare x y


  let pp f var =
    let pp_ampersand f = function ProgramVar _ -> F.pp_print_string f "&" | LogicalVar _ -> () in
    let pp_proc_name f var =
      let open IOption.Let_syntax in
      match Var.get_pvar var >>= Pvar.get_declaring_function with
      | Some pvar_proc_name
        when not
               (Option.exists (PulseContext.proc_desc ()) ~f:(fun proc_desc ->
                    Procname.equal (Procdesc.get_proc_name proc_desc) pvar_proc_name ) ) ->
          F.fprintf f "|%a" Procname.pp pvar_proc_name
      | _ ->
          ()
    in
    F.fprintf f "%a%a%a" pp_ampersand var Var.pp var pp_proc_name var
end

module M = PrettyPrintable.MakePPMonoMap (VarAddress) (ValueOrigin)

let yojson_of_t m = [%yojson_of: (VarAddress.t * ValueOrigin.t) list] (M.bindings m)

let canonicalize ~get_var_repr stack =
  let exception AliasingContradiction in
  try
    let (_allocated, changed), stack' =
      M.fold_mapi stack ~init:(AbstractValue.Set.empty, false)
        ~f:(fun var (allocated, changed) vo ->
          let addr = ValueOrigin.value vo in
          let addr' = get_var_repr addr in
          if Var.is_pvar var && AbstractValue.Set.mem addr' allocated then (
            L.d_printfln
              "CONTRADICTION: %a = %a makes two stack variables' addresses equal (%a=%a) in %a@\n"
              AbstractValue.pp addr AbstractValue.pp addr' AbstractValue.pp addr Var.pp var M.pp
              stack ;
            raise_notrace AliasingContradiction ) ;
          let allocated =
            if Var.is_pvar var then AbstractValue.Set.add addr' allocated else allocated
          in
          if phys_equal addr addr' then ((allocated, changed), vo)
          else
            let changed = true in
            ((allocated, changed), ValueOrigin.with_value addr' vo) )
    in
    Sat (if changed then stack' else stack)
  with AliasingContradiction -> Unsat


let subst_var (v, v') stack =
  canonicalize stack ~get_var_repr:(fun addr -> if AbstractValue.equal v addr then v' else addr)


include M

let compare = M.compare ValueOrigin.compare

let equal = M.equal ValueOrigin.equal

let pp fmt m =
  let pp_item fmt (var_address, v) =
    F.fprintf fmt "%a=%a" VarAddress.pp var_address ValueOrigin.pp v
  in
  PrettyPrintable.pp_collection ~pp_item fmt (M.bindings m)


(* copy/pasted from the .mli because ocaml *)
module type S = sig
  include PrettyPrintable.PPMonoMap with type key = Var.t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end
