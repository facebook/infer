(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Hacky script to turn the json output of [hackc facts] on hhi files into Textual type definitions.
   Lots of TODOs:
   - Although we process ["kind": "Alias"], hackc doesn't currently output the definitions
     of aliases, which we'll want to add.
   - We're not also generating the definitions of so-called static companion types, which will cause
     problems with static calls and uses of [classname].
   - Blanket running this on every hhi may generate some definitions that clash with ones we want to write
     by hand because they're important (e.g. [vec], [Awaitable]). My current approach is just to run this manually, fix up the output,
     and commit the result, but we should make it more automated. Probably by just dropping the hhi information
     for any type that also gets defined in models.sil.
   - It's possible that a different data source would work better than [hackc], but this is easy to parse. Longer term
     I'd prefer infer to talk directly to [hh] as the source of truth.
*)
open Core
module YBU = Yojson.Basic.Util

let usage_msg = "hhidecls file"

type tykind = Class | Interface | Alias | Trait

let str_to_tykind s =
  match s with
  | "Class" ->
      Some Class
  | "Interface" ->
      Some Interface
  | "TypeAlias" ->
      Some Alias
  | "Trait" ->
      Some Trait
  | _ ->
      None


(* Observe that the kind strings used in Textual and the strings used in json facts are slightly different *)
let tykind_to_str tk =
  match tk with
  | Class ->
      "class"
  | Interface ->
      "interface"
  | Alias ->
      "typedef"
  | Trait ->
      "trait"


let print_tykind k_opt =
  match k_opt with None -> () | Some tk -> printf " = .kind=\"%s\" " (tykind_to_str tk)


(* Here it's single backslash because the \\ in the file turns into \ in the string *)
let single_backslash_to_double_colon s = String.substr_replace_all s ~pattern:"\\" ~with_:"::"

(* Note that "extends" and "implements" are conflated in both json facts and Textual
   Need to use kind to disambiguate
*)
let print_base_types bt_json =
  match bt_json with
  | [] ->
      ()
  | _ ->
      printf " extends %s "
        (String.concat ~sep:", "
           (List.map bt_json ~f:(fun j -> j |> YBU.to_string |> single_backslash_to_double_colon)) )


let abstract_flag_value = 1

let final_flag_value = 2

let print_flags fv kind_opt =
  ( match kind_opt with
  (* json seems to mark all interfaces also abstract, which makes sense but I don't think
     it matches what we do elsewhere, so don't *)
  | Some Interface ->
      ()
  | _ ->
      if fv land abstract_flag_value > 0 then printf ".abstract " ) ;
  if fv land final_flag_value > 0 then printf ".final "


(* print the textual declaration of a json value representing its decl *)
let print_type (tyname, ty_json) =
  let tyname = single_backslash_to_double_colon tyname in
  let base_types = YBU.member "base_types" ty_json |> YBU.to_list in
  let kind_opt = YBU.member "kind" ty_json |> YBU.to_string |> str_to_tykind in
  let flags = YBU.member "flags" ty_json |> YBU.to_int in
  match kind_opt with
  (* Skip type aliases until infer knows how to parse them
     and we can get their definitions from somewhere so as to
     be able to do something useful with them.
     Also skip traits because they're not (I think) relevant here *)
  | Some Alias ->
      printf "// ALIAS: type %s should be defined manually above\n" tyname
  | Some Trait ->
      ()
  | _ ->
      printf "type %s" tyname ;
      print_base_types base_types ;
      print_tykind kind_opt ;
      print_flags flags kind_opt ;
      printf "{}\n"


let () =
  let to_check = ref None in
  let set_file fname = to_check := Some fname in
  Arg.parse [] set_file usage_msg ;
  let buf =
    match !to_check with None -> In_channel.(input_all stdin) | Some fn -> In_channel.read_all fn
  in
  let json = Yojson.Basic.from_string buf in
  let types = json |> YBU.member "types" |> YBU.to_assoc in
  List.iter types ~f:print_type ;
  exit 0
