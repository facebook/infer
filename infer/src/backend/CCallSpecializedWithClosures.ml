(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type formal_annot = Mangled.t * Typ.t * Annot.Item.t [@@deriving compare]

let has_closure actual_params =
  List.exists actual_params ~f:(fun (exp, _) ->
      match exp with Exp.Closure c -> Procname.is_objc_block c.name | _ -> false )


let should_specialize actual_params call_flags =
  let block_is_receiver actual_params =
    Int.equal (List.length actual_params) 1 && call_flags.CallFlags.cf_virtual
  in
  has_closure actual_params && not (block_is_receiver actual_params)


(* name for the specialized method instantiated with closure arguments *)
let pname_with_closure_args callee_pname actual_params =
  let block_args =
    List.filter_map actual_params ~f:(function
      | Exp.Closure cl, _ when Procname.is_objc_block cl.name ->
          Some (Procname.block_of_procname cl.name)
      | _ ->
          None )
  in
  Procname.with_block_parameters callee_pname block_args


let make_formals_to_procs_and_new_formals formals actual_params =
  match
    List.fold2 formals actual_params ~init:Mangled.Map.empty ~f:(fun map (mangled, _, _) actual ->
        match actual with
        | Exp.Closure closure, _ ->
            let captured_as_formals =
              List.map
                ~f:(fun (_, pvar, typ, capture_mode) -> {CapturedVar.pvar; typ; capture_mode})
                closure.captured_vars
            in
            Mangled.Map.add mangled (closure.name, captured_as_formals) map
        | _ ->
            map )
  with
  | Ok map ->
      Some map
  | Unequal_lengths ->
      None


let get_captured actual_params =
  ClosuresSubstitution.map_args_captured_vars actual_params ~f:(fun c ->
      List.map c.captured_vars ~f:(fun (_, pvar, typ, capture_mode) ->
          {CapturedVar.pvar; capture_mode; typ} ) )


let is_objc_setter proc_desc =
  let attributes = Procdesc.get_attributes proc_desc in
  match attributes.ProcAttributes.objc_accessor with Some (Objc_setter _) -> true | _ -> false


let is_initializer proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  Procname.is_constructor proc_name


let is_dispatch_model proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  ObjCDispatchModels.is_model proc_name


let replace_with_specialize_methods instr =
  match instr with
  | Sil.Call (ret, Exp.Const (Const.Cfun callee_pname), actual_params, loc, flags)
    when should_specialize actual_params flags -> (
    match Procdesc.load callee_pname with
    (*TODO(T74127433): This specialization works well only when the we specialize methods that take a block
      parameter and then run the block. It doesn't work well when the block is instead stored in
      a field. This case will be left as future work, and we won't specialize common cases where this
      happens such as setters or initializers. *)
    | Some proc_desc
      when (not (is_objc_setter proc_desc))
           && (not (is_initializer proc_desc))
           && not (is_dispatch_model proc_desc) -> (
        let callee_attributes = Procdesc.get_attributes proc_desc in
        match make_formals_to_procs_and_new_formals callee_attributes.formals actual_params with
        | Some formals_to_procs_and_new_formals -> (
            let specialized_pname = pname_with_closure_args callee_pname actual_params in
            let new_attributes =
              { callee_attributes with
                specialized_with_blocks_info=
                  Some {orig_proc= callee_pname; formals_to_procs_and_new_formals}
              ; captured= get_captured actual_params @ callee_attributes.captured
              ; is_defined= true
              ; proc_name= specialized_pname }
            in
            (* To avoid duplicated additions on a specialized procname, it does a membership check.
               This may happen when there are multiple function calls with the same callees and the
               same closure parameters.  For the following additions, we can simply ignore them,
               because the function bodies of the same procname must be the same.

               Here, it creates an empty procdesc temporarily.  The function body will be filled
               later by [ClosureSubstSpecializedMethod]. *)
            let specialized_instr =
              Sil.Call (ret, Exp.Const (Const.Cfun specialized_pname), actual_params, loc, flags)
            in
            match Procdesc.load specialized_pname with
            | Some _ ->
                specialized_instr (* already exists*)
            | None -> (
              match Procdesc.load callee_pname with
              | Some orig_pdesc when (Procdesc.get_attributes orig_pdesc).is_defined ->
                  let specialized_pdesc = Procdesc.from_proc_attributes new_attributes in
                  Attributes.store ~proc_desc:(Some specialized_pdesc) new_attributes ;
                  specialized_instr
              | _ ->
                  instr (* No procdesc to specialize: either non-existant or not defined *) ) )
        | None ->
            instr )
    | _ ->
        instr )
  | _ ->
      instr


let process proc_desc =
  (* For each procdesc:
     1. If we are a specialized pdesc:
      1.1. Copy original procdesc
      1.2. Update blocks' uses (at instruction level)
     2. Update calls' closure arguments (at expression level)
     3. Update calls with closures as arguments to specialized calls
     4. Create procdescs for specialized callees if they don't already exist
     5. Update closure calls (at expression level)
  *)
  (* Create a specialized copy of proc_desc's orig_pdesc (if it exists).
     Specialize at instruction level: [foo(f)] when [f = block] replaces every
     use of [f] (calls and as argument) with the corresponding [block] and its
     captured arguments. *)
  ClosureSubstSpecializedMethod.process proc_desc ;
  (* Replace each indirect use of closure (as argument) with a direct use of closure.
     e.g. [foo(a, b, c)] when b = Closure(closure_b, ...) becomes
          [foo(a, Closure(closure_b, ...), c)] *)
  ClosuresSubstitution.process_closure_param proc_desc ;
  (* Replace every function call taking a known closure as argument with specialized calls
     and create the corresponding (empty for now) specialized pdescs.
     e.g. [foo(a, Closure(closure_b, captured_args...), c)] becomes
      [foo\[closure_b\](a, captured_args..., Closure(closure_b, captured_args...), c)]
      and the pdesc for [foo\[closure_b\]\ is created.
     Note:
      Captured args placement in the list of args depends on the set ordering).
      The newly created pdescs (stored in [need_specalization]) will be filled out by
      [ClosureSubstSpecializedMethod.process] above when it will be their turn to be
      (pre)processed.
  *)
  Procdesc.replace_instrs proc_desc ~f:(fun _node instr -> replace_with_specialize_methods instr)
  |> ignore ;
  (* Replace [ident] calls by [closure] calls with added [closure]'s arguments
     if [ident = closure].
     e.g. [foo(a, b, c)] when [foo = Closure(closure_foo, captured_args...)] becomes
          [closure_foo(captured_args..., a, b, c)] *)
  ClosuresSubstitution.process_closure_call proc_desc
