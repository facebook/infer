(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
include AbstractDomain.Flat (AccessExpression)

let is_supported_procname pname = Procname.is_c pname || Procname.is_cpp_method pname

let rec accexp_of_hilexp (exp : HilExp.t) =
  match exp with
  | AccessExpression access_expr ->
      Some access_expr
  | Cast (_, exp) ->
      accexp_of_hilexp exp
  | _ ->
      None


let is_rooted_at_formal_or_global formals access_expr =
  match AccessExpression.get_base access_expr with
  | (ProgramVar pvar, _) as base ->
      Pvar.is_global pvar || FormalMap.is_formal base formals
  | LogicalVar _, _ ->
      false


let assign formals ~lhs ~rhs astate =
  if not (AccessExpression.is_return_var lhs) then astate
  else if HilExp.is_null_literal rhs then
    (* no access or lock goes through a null pointer, so it does not contradict any alias *)
    bottom
  else
    match accexp_of_hilexp rhs with
    | Some access_expr when is_rooted_at_formal_or_global formals access_expr ->
        v access_expr
    | _ ->
        top


let may_modify_root proc_desc alias =
  match AccessExpression.get_base alias with
  | ProgramVar root, _ when not (Pvar.is_global root) ->
      let has_root exp = Sequence.exists (Exp.program_vars exp) ~f:(Pvar.equal root) in
      (* the formal is written to, or its address escapes *)
      Procdesc.find_map_instrs proc_desc ~f:(fun (instr : Sil.instr) ->
          match instr with
          | Store {e1; e2} ->
              Option.some_if (has_root e1 || has_root e2) ()
          | Call (_, _, actuals, _, _) ->
              Option.some_if (List.exists actuals ~f:(fun (actual, _) -> has_root actual)) ()
          | Load _ | Prune _ | Metadata _ ->
              None )
      |> Option.is_some
  | _ ->
      false


let smart_pointer_accessors =
  QualifiedCppName.Match.of_fuzzy_qual_names
    [ "std::__shared_ptr::get"
    ; "std::__shared_ptr_access::operator*"
    ; "std::__shared_ptr_access::operator->"
    ; "std::shared_ptr::get"
    ; "std::shared_ptr::operator*"
    ; "std::shared_ptr::operator->"
    ; "std::unique_ptr::get"
    ; "std::unique_ptr::operator*"
    ; "std::unique_ptr::operator->" ]


let to_summary proc_desc astate =
  let proc_name = Procdesc.get_proc_name proc_desc in
  if
    Procname.is_cpp_method proc_name
    && QualifiedCppName.Match.match_qualifiers smart_pointer_accessors
         (Procname.get_qualifiers proc_name)
  then
    (* a smart pointer stands for the pointer it holds, so that paths through it, e.g.
       [this->p->x], do not depend on the implementation of the standard library *)
    List.hd (Procdesc.get_pvar_formals proc_desc)
    |> Option.bind ~f:(fun (this, typ) ->
        AccessExpression.add_access (AccessExpression.base (Var.of_pvar this, typ)) Dereference )
  else if is_supported_procname proc_name && Typ.is_pointer (Procdesc.get_ret_type proc_desc) then
    (* a formal-rooted alias is substituted with the actual, i.e. the value of the formal on entry *)
    get astate |> Option.filter ~f:(fun alias -> not (may_modify_root proc_desc alias))
  else None


let subst ~callee actuals alias =
  let ((var, _) as base) = AccessExpression.get_base alias in
  if Var.is_global var then Some alias
  else
    Attributes.load callee
    |> Option.bind ~f:(fun callee_attrs ->
        FormalMap.get_formal_index base (FormalMap.make callee_attrs) )
    |> Option.bind ~f:(List.nth actuals)
    |> Option.bind ~f:accexp_of_hilexp
    |> Option.bind ~f:(fun actual -> AccessExpression.append ~onto:actual alias)
