(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module ProfilerSample = Caml.Set.Make (Typ.Procname)

module JNI = struct
  (* https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html *)
  type t =
    | Boolean
    | Byte
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double
    | Void
    (* FullyQualifiedClass is split between (package, class) *)
    | FullyQualifiedClass of (string * string)
    | Array of t
    | Method of (t list * t)
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let void_method_with_no_arguments = "()V"

  type non_terminal_symbol = SymArray | SymMethodOpen | SymMethodClose | SymMethod of t list

  type symbol = Terminal of t | NonTerminal of non_terminal_symbol

  let to_fully_qualified_class str =
    let open String in
    rstrip ~drop:(Char.equal ';') str |> tr ~target:'/' ~replacement:'.' |> rsplit2_exn ~on:'.'


  let rec pp fmt jni =
    match jni with
    | Boolean ->
        Format.pp_print_char fmt 'Z'
    | Byte ->
        Format.pp_print_char fmt 'B'
    | Char ->
        Format.pp_print_char fmt 'C'
    | Short ->
        Format.pp_print_char fmt 'S'
    | Int ->
        Format.pp_print_char fmt 'I'
    | Long ->
        Format.pp_print_char fmt 'J'
    | Float ->
        Format.pp_print_char fmt 'F'
    | Double ->
        Format.pp_print_char fmt 'D'
    | Void ->
        Format.pp_print_char fmt 'V'
    | FullyQualifiedClass (pkg, cl) ->
        let pkg' = String.tr ~target:'.' ~replacement:'/' pkg in
        Format.fprintf fmt "L%s/%s;" pkg' cl
    | Array t ->
        Format.fprintf fmt "[%a" pp t
    | Method (args, ret_typ) ->
        Format.fprintf fmt "(%a)%a" (Pp.seq ~sep:"" pp) args pp ret_typ


  let rec to_java_type jni =
    let make = Typ.Name.Java.Split.make in
    match jni with
    | Boolean ->
        make "bool"
    | Byte ->
        make "byte"
    | Char ->
        make "char"
    | Short ->
        make "short"
    | Int ->
        make "int"
    | Long ->
        make "long"
    | Float ->
        make "float"
    | Double ->
        make "double"
    | Void ->
        make "void"
    | FullyQualifiedClass (pkg, cl) ->
        make ~package:pkg cl
    | Array typ ->
        let java_type = to_java_type typ in
        let typ_str = Typ.Name.Java.Split.type_name java_type in
        make ?package:(Typ.Name.Java.Split.package java_type) (Printf.sprintf "%s[]" typ_str)
    | Method _ ->
        L.(die UserError "Cannot express a method as a Procname.Java.java_type")


  let tokenize input =
    let rec tokenize_aux input acc =
      match input with
      | [] ->
          List.rev acc
      | c :: cs -> (
        match c with
        | '(' ->
            tokenize_aux cs (NonTerminal SymMethodOpen :: acc)
        | ')' ->
            tokenize_aux cs (NonTerminal SymMethodClose :: acc)
        | '[' ->
            tokenize_aux cs (NonTerminal SymArray :: acc)
        | 'Z' ->
            tokenize_aux cs (Terminal Boolean :: acc)
        | 'B' ->
            tokenize_aux cs (Terminal Byte :: acc)
        | 'C' ->
            tokenize_aux cs (Terminal Char :: acc)
        | 'S' ->
            tokenize_aux cs (Terminal Short :: acc)
        | 'I' ->
            tokenize_aux cs (Terminal Int :: acc)
        | 'J' ->
            tokenize_aux cs (Terminal Long :: acc)
        | 'F' ->
            tokenize_aux cs (Terminal Float :: acc)
        | 'D' ->
            tokenize_aux cs (Terminal Double :: acc)
        | 'V' ->
            tokenize_aux cs (Terminal Void :: acc)
        | 'L' ->
            let semicolon_pos =
              match List.findi cs ~f:(fun _ c -> Char.equal ';' c) with
              | None ->
                  let open L in
                  die UserError
                    "Cannot find a semicolon symbol to delimit the L token. Failed parsing input" c
              | Some (pos, _) ->
                  pos
            in
            let consumed_input, new_input = List.split_n cs (semicolon_pos + 1) in
            let fully_qualified_class =
              String.of_char_list consumed_input |> to_fully_qualified_class
            in
            tokenize_aux new_input (Terminal (FullyQualifiedClass fully_qualified_class) :: acc)
        | c ->
            L.(die UserError "Unrecognized char '%c' while reading the input sequence" c) )
    in
    let input_chars = String.to_list input in
    tokenize_aux input_chars []


  (** Very simple minded: keep reducing until no more non-terminals are left. 
      Fail if no reductions happened during a scan *)
  let reduce symbols =
    let rec reduce_aux ~symbols ~unchanged_symbols ~in_method ~jnis_in_method ~jnis =
      let tl_or_empty l = match l with [] -> [] | [_] -> [] | _ :: tl -> tl in
      let collected_symbols =
        not (List.is_empty unchanged_symbols && List.is_empty jnis_in_method)
      in
      let all_collected_symbols_so_far () =
        if in_method then
          let terminals_in_method = List.map ~f:(fun jni -> Terminal jni) jnis_in_method in
          terminals_in_method @ unchanged_symbols
        else unchanged_symbols
      in
      match symbols with
      | [] ->
          if collected_symbols then
            L.(die UserError "No symbols were reduced during a scan, failed parsing input")
          else (* no more symbols in input, terminate *) List.rev jnis
      | Terminal t :: tl when (not collected_symbols) && not in_method ->
          reduce_aux ~symbols:tl ~unchanged_symbols ~in_method ~jnis_in_method ~jnis:(t :: jnis)
      | NonTerminal (SymMethod method_jnis) :: Terminal t :: tl ->
          let transformed_symbols = Terminal (Method (method_jnis, t)) :: tl in
          let new_symbols =
            List.rev_append (all_collected_symbols_so_far ()) transformed_symbols
          in
          reduce_aux ~symbols:new_symbols ~unchanged_symbols:[] ~in_method:false ~jnis_in_method:[]
            ~jnis
      | (NonTerminal SymMethodOpen as nt) :: tl ->
          reduce_aux ~symbols:tl
            ~unchanged_symbols:(nt :: all_collected_symbols_so_far ())
            ~in_method:true ~jnis_in_method:[] ~jnis
      | NonTerminal SymArray :: Terminal t :: tl ->
          let transformed_symbols = Terminal (Array t) :: tl in
          let new_symbols =
            List.rev_append (all_collected_symbols_so_far ()) transformed_symbols
          in
          reduce_aux ~symbols:new_symbols ~unchanged_symbols:[] ~in_method:false ~jnis_in_method:[]
            ~jnis
      | (NonTerminal SymArray as nt) :: tl ->
          reduce_aux ~symbols:tl
            ~unchanged_symbols:(nt :: all_collected_symbols_so_far ())
            ~in_method:false ~jnis_in_method:[] ~jnis
      | NonTerminal SymMethodClose :: tl ->
          let new_method_non_terminal = NonTerminal (SymMethod (List.rev jnis_in_method)) in
          (* the first symbol in unchanged_symbols is the SymMethodOpen, remove it *)
          let new_unchanged_symbols = tl_or_empty unchanged_symbols in
          let new_tokens = List.rev_append new_unchanged_symbols (new_method_non_terminal :: tl) in
          reduce_aux ~symbols:new_tokens ~unchanged_symbols:[] ~in_method:false ~jnis_in_method:[]
            ~jnis
      | t :: tl ->
          let unchanged_symbols' =
            if in_method then unchanged_symbols else t :: unchanged_symbols
          in
          let collected_in_method' =
            if in_method then
              (* at this point, all tokens inside a method block are terminals *)
              match t with
              | Terminal s ->
                  s :: jnis_in_method
              | NonTerminal _ ->
                  let open L in
                  die UserError
                    "Unexpected non-terminal symbol found within a method block, failed parsing \
                     input"
            else jnis_in_method
          in
          reduce_aux ~symbols:tl ~unchanged_symbols:unchanged_symbols' ~in_method
            ~jnis_in_method:collected_in_method' ~jnis
    in
    reduce_aux ~symbols ~unchanged_symbols:[] ~in_method:false ~jnis_in_method:[] ~jnis:[]


  let parse_str = Fn.compose reduce tokenize

  let parse_method_str str =
    match parse_str str with
    | [Method m] ->
        m
    | _ ->
        L.(die UserError "'%s' did not parse as one JNI method signature" str)


  module VISIBLE_FOR_TESTING_DO_NOT_USE_DIRECTLY = struct
    type nonrec t = t =
      | Boolean
      | Byte
      | Char
      | Short
      | Int
      | Long
      | Float
      | Double
      | Void
      (* FullyQualifiedClass is split between (package, class) *)
      | FullyQualifiedClass of (string * string)
      | Array of t
      | Method of (t list * t)

    let compare = compare

    let equal = equal

    let parse_str = parse_str

    let parse_method_str = parse_method_str

    let to_java_type = to_java_type

    let pp = pp
  end
end

let create_procname ~classname ~methodname ~signature =
  let name = Typ.Name.Java.from_string classname in
  let args, ret_typ = JNI.parse_method_str signature in
  let java_type_args = List.map ~f:JNI.to_java_type args in
  let java_type_ret_typ =
    if
      String.equal methodname Typ.Procname.Java.constructor_method_name
      || String.equal methodname Typ.Procname.Java.class_initializer_method_name
    then None
    else Some (JNI.to_java_type ret_typ)
  in
  Typ.Procname.Java
    (Typ.Procname.Java.make name java_type_ret_typ methodname java_type_args
       Typ.Procname.Java.Non_Static)


type labeled_profiler_sample = string * ProfilerSample.t [@@deriving compare]

let equal_labeled_profiler_sample = [%compare.equal: labeled_profiler_sample]

let from_java_profiler_samples j ~use_signature =
  let process_methods methods =
    ProfilerSample.of_list
      (List.map
         ~f:(fun {Java_profiler_samples_t.classname; methodname; signature} ->
           let signature =
             if use_signature then signature else JNI.void_method_with_no_arguments
           in
           create_procname ~classname ~methodname ~signature )
         methods)
  in
  List.map j ~f:(fun {Java_profiler_samples_t.test; methods} -> (test, process_methods methods))


let from_json_string str ~use_signature =
  from_java_profiler_samples
    (Java_profiler_samples_j.java_profiler_samples_of_string str)
    ~use_signature


let from_json_file file ~use_signature =
  from_java_profiler_samples
    (Atdgen_runtime.Util.Json.from_file Java_profiler_samples_j.read_java_profiler_samples file)
    ~use_signature
