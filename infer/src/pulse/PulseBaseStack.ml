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

let current_proc_name = ref Procname.Linters_dummy_method

let () = AnalysisGlobalState.register_ref_with_proc_name current_proc_name ~init:Fn.id

module VarAddress = struct
  include Var

  let pp f var =
    let pp_ampersand f = function ProgramVar _ -> F.pp_print_string f "&" | LogicalVar _ -> () in
    let pp_proc_name f var =
      let open IOption.Let_syntax in
      match Var.get_pvar var >>= Pvar.get_declaring_function with
      | Some pvar_proc_name when not (Procname.equal !current_proc_name pvar_proc_name) ->
          F.fprintf f "|%a" Procname.pp pvar_proc_name
      | _ ->
          ()
    in
    F.fprintf f "%a%a%a" pp_ampersand var Var.pp var pp_proc_name var
end

module AddrHistPair = struct
  type t = AbstractValue.t * ValueHistory.t [@@deriving compare, equal, yojson_of]

  let pp f addr_trace =
    if Config.debug_level_analysis >= 3 then
      Pp.pair ~fst:AbstractValue.pp ~snd:ValueHistory.pp f addr_trace
    else AbstractValue.pp f (fst addr_trace)
end

module M = PrettyPrintable.MakePPMonoMap (VarAddress) (AddrHistPair)

let yojson_of_t m = [%yojson_of: (VarAddress.t * AddrHistPair.t) list] (M.bindings m)

let canonicalize ~get_var_repr stack =
  let exception AliasingContradiction in
  try
    let (_allocated, changed), stack' =
      M.fold_mapi stack ~init:(AbstractValue.Set.empty, false)
        ~f:(fun var (allocated, changed) ((addr, hist) as addr_hist) ->
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
          if phys_equal addr addr' then ((allocated, changed), addr_hist)
          else
            let changed = true in
            ((allocated, changed), (addr', hist)) )
    in
    Sat (if changed then stack' else stack)
  with AliasingContradiction -> Unsat


let subst_var (v, v') stack =
  canonicalize stack ~get_var_repr:(fun addr -> if AbstractValue.equal v addr then v' else addr)


include M

let compare = M.compare AddrHistPair.compare

let equal = M.equal AddrHistPair.equal

let pp fmt m =
  let pp_item fmt (var_address, v) =
    F.fprintf fmt "%a=%a" VarAddress.pp var_address AddrHistPair.pp v
  in
  PrettyPrintable.pp_collection ~pp_item fmt (M.bindings m)
