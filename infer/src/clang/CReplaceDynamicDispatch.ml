(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let dispatch_json_opt =
  (fun x -> match x with Some y -> Some (Yojson.Basic.from_file y) | None -> None)
    Config.dynamic_dispatch_json_file_path


(* Accumulate dynamic dispatch context in the node *)
let eval_instr astate (instr : Sil.instr) =
  match instr with
  | Load {id; e} ->
      (* e.g., n$1 = $mockptr_HandleIntentForKey:_fn_ *)
      Exp.Map.add (Var id) e astate
  | Call ((id, _), Const (Cfun callee), args, _, _) ->
      (* e.g., n$2 = _fun_NSString.stringWithUTF8String:("dispatch_1":char* const ) *)
      if String.equal (Procname.get_method callee) "stringWithUTF8String:" then
        match args with (e, _) :: _ -> Exp.Map.add (Var id) e astate | _ -> astate
      else astate
  | _ ->
      astate


let has_dispatch_key key json =
  List.exists (Yojson.Basic.Util.keys json) ~f:(fun x -> String.( = ) x key)


let lookup dispatch_call dispatch_key json =
  let dispatch_calls = Yojson.Basic.Util.keys json in
  List.find dispatch_calls ~f:(fun suffix ->
      if String.is_suffix dispatch_call ~suffix then
        let m = json |> Yojson.Basic.Util.member suffix in
        has_dispatch_key dispatch_key m
      else false )
  |> Option.map ~f:(fun x ->
         json |> Yojson.Basic.Util.member x
         |> Yojson.Basic.Util.member dispatch_key
         |> Yojson.Basic.Util.to_string )


let get_replaced_method_name call_opt key json =
  Option.bind call_opt ~f:(fun call -> lookup call key json)


let get_dispatch_call_name context exp =
  let exp_ = Option.value (Exp.Map.find_opt exp context) ~default:exp in
  match exp_ with
  | Exp.Lvar pvar ->
      let name = Pvar.get_simplified_name pvar in
      Some
        ( if String.is_prefix name ~prefix:"mockptr_" then
            String.drop_prefix name (String.length "mockptr_")
          else name )
  | Exp.Const (Cfun callee) ->
      Some (Procname.get_method callee)
  | _ ->
      None


let get_dispatch_key_name exp =
  match exp with
  | Exp.Lvar pvar ->
      Some (Pvar.get_simplified_name pvar)
  | Exp.Const (Cstr str) ->
      Some str
  | _ ->
      None


(* Replace calls like HandleIntentForKey(@"dispatch1", args) with dispatched1(args), where
 * HandleIntentForKey is the dispatch_call_opt,
 * dispatch1 is the dispatch_key,
 * dispatched1 is the dispatched_method_name *)
let get_replaced_instr context dispatch_json dispatch_call_opt args ret_id_typ loc flags =
  let open IOption.Let_syntax in
  match args with
  | [] ->
      None
  | (first_arg, _) :: args_after_first ->
      let* dispatch_key_name = Exp.Map.find_opt first_arg context >>= get_dispatch_key_name in
      let+ dispatched_method_name =
        get_replaced_method_name dispatch_call_opt dispatch_key_name dispatch_json
      in
      let pname = Procname.from_string_c_fun dispatched_method_name in
      let exp = Exp.Const (Cfun pname) in
      L.debug Capture Verbose "replacing call with %s @\n" dispatched_method_name ;
      Sil.Call (ret_id_typ, exp, args_after_first, loc, flags)


let replace_calls dispatch_json _ proc_desc =
  let replace_dispatch_call _ context instr =
    match (instr : Sil.instr) with
    | Call (ret_id_typ, callee, args, loc, flags) -> (
      match
        let dispatch_call_name = get_dispatch_call_name context callee in
        get_replaced_instr context dispatch_json dispatch_call_name args ret_id_typ loc flags
      with
      | Some replaced ->
          replaced
      | None ->
          instr )
    | _ ->
        instr
  in
  let update_context = eval_instr in
  let context_at_node _ = Exp.Map.empty in
  Procdesc.replace_instrs_using_context proc_desc ~f:replace_dispatch_call ~update_context
    ~context_at_node
  |> ignore


let process cfg =
  match dispatch_json_opt with
  | None ->
      ()
  | Some dispatch_json ->
      Procname.Hash.iter (replace_calls dispatch_json) cfg
