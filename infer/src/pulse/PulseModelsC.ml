(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module DSL = PulseModelsDSL

let free deleted_access : model = Basic.free_or_delete `Free CFree deleted_access

let invalidate path access_path location cause addr_trace : unit DSL.model_monad =
  let open DSL.Syntax in
  PulseOperations.invalidate path access_path location cause addr_trace |> exec_command


let return_null_dsl : unit DSL.model_monad =
  let open DSL.Syntax in
  let* {path; location; ret= ret_id, _} = get_data in
  let* ret_addr = fresh ~more:"(null case)" () in
  assign_ret ret_addr @@> and_eq_int ret_addr IntLit.zero
  @@> invalidate path
        (StackAddress (Var.of_id ret_id, snd ret_addr))
        location (ConstantDereference IntLit.zero) ret_addr


let alloc_common_dsl ~null_case ~initialize allocator size_exp_opt : unit DSL.model_monad =
  let open DSL.Syntax in
  let astate_alloc = Basic.return_alloc_not_null allocator ~initialize size_exp_opt in
  if null_case then disj [astate_alloc; return_null_dsl] else astate_alloc


let alloc_common ~null_case ~initialize ~desc allocator size_exp_opt : model =
  let open DSL.Syntax in
  start_named_model desc @@ fun () -> alloc_common_dsl ~null_case ~initialize allocator size_exp_opt


let malloc ~null_case size_exp =
  alloc_common ~null_case ~initialize:false ~desc:"malloc" CMalloc (Some size_exp)


let custom_malloc ~null_case size_exp model_data astate =
  alloc_common ~null_case ~initialize:false ~desc:"custom malloc"
    (CustomMalloc model_data.callee_procname) (Some size_exp) model_data astate


let custom_alloc_not_null desc model_data astate =
  alloc_common ~initialize:false ~null_case:false ~desc (CustomMalloc model_data.callee_procname)
    None model_data astate


(* A failed realloc returns NULL and leaves the original block allocated, so only the success case
   frees [pointer]. A zero [size] is not special-cased: C17 leaves it implementation-defined (glibc
   and scudo free [pointer] and return NULL) and C23 makes it undefined. *)
let realloc_common ~null_case ~desc allocator pointer size : model =
  let open DSL.Syntax in
  start_named_model desc
  @@ fun () ->
  let success =
    lift_to_monad (free pointer)
    @@> alloc_common_dsl ~null_case:false ~initialize:false allocator (Some size)
  in
  if null_case then disj [success; return_null_dsl] else success


let realloc = realloc_common ~desc:"realloc" CRealloc

let custom_realloc pointer size data astate =
  realloc_common ~desc:"custom realloc" (CustomRealloc data.callee_procname) pointer size data
    astate


let call_c_function_ptr {FuncArg.arg_payload= function_ptr} actuals : model =
 fun {path; analysis_data; location; ret= (ret_id, _) as ret; dispatch_call_eval_args} astate
     non_disj ->
  let callee_proc_name_opt =
    match PulseArithmetic.get_dynamic_type (ValueOrigin.value function_ptr) astate with
    | Some {typ= {desc= Typ.Tstruct (Typ.CFunction csig)}} ->
        Some (Procname.C csig)
    | _ ->
        None
  in
  match callee_proc_name_opt with
  | Some callee_proc_name ->
      dispatch_call_eval_args analysis_data path ret (Const (Cfun callee_proc_name)) actuals
        location CallFlags.default astate non_disj (Some callee_proc_name)
  | None ->
      (* we don't know what procname this function pointer resolves to *)
      let res =
        (* dereference call expression to catch nil issues *)
        let<+> astate, _ =
          PulseOperations.eval_access path Read location
            (ValueOrigin.addr_hist function_ptr)
            Dereference astate
        in
        let desc = Procname.to_string BuiltinDecl.__call_c_function_ptr in
        let hist = Hist.single_event (Hist.call_event path location desc) in
        let astate = PulseOperations.havoc_id ret_id hist astate in
        let astate =
          AbductiveDomain.add_need_dynamic_type_specialization (ValueOrigin.value function_ptr)
            astate
        in
        let astate =
          let unknown_effect = Attribute.UnknownEffect (Model desc, hist) in
          List.fold actuals ~init:astate ~f:(fun acc FuncArg.{arg_payload= actual; typ} ->
              let actual = ValueOrigin.value actual in
              let acc =
                if
                  Config.pulse_havoc_arguments && Typ.is_pointer typ
                  && not (Typ.is_ptr_to_const typ)
                then AbductiveDomain.apply_unknown_effect hist actual acc
                else acc
              in
              AddressAttributes.add_one actual unknown_effect acc )
        in
        astate
      in
      (res, non_disj)


(* `pthread_once(control, init)` runs the `init` callback exactly once
   across all threads sharing `control`. For Pulse's value-tracking we
   treat it as an unconditional invocation of `init`: the side-effects
   of the initialiser (e.g. setting a global) MUST be visible to
   callers for the canonical lazy-init / singleton pattern to be
   provable. We don't model the once-guard itself.

   The `int` return value (0 on success) is left to whatever
   [call_c_function_ptr] writes for the inner call's return (typically
   void / non-deterministic). Most production callers ignore the
   return; a more precise treatment can be added later if needed. *)
let pthread_once init_func : model = call_c_function_ptr init_func []

(** a few models from (g)libc and beyond *)
include struct
  open DSL.Syntax

  let assume_not_null pointer =
    start_model @@ fun () -> prune_ne_zero (to_aval pointer) @@> assign_ret (to_aval pointer)


  let valid_arg arg : model = start_model @@ fun () -> check_valid arg

  let null_or_valid_arg arg =
    start_model
    @@ fun () -> disj [prune_eq_zero (to_aval arg); prune_ne_zero (to_aval arg) @@> check_valid arg]


  let valid_args2 arg1 arg2 : model = start_model @@ fun () -> check_valid arg1 @@> check_valid arg2

  let valid_and_null_or_valid_args arg1 arg2 : model =
    start_model
    @@ fun () -> lift_to_monad (valid_arg arg1) @@> lift_to_monad (null_or_valid_arg arg2)


  let non_det_ret : model = start_model @@ fun () -> assign_ret @= fresh ()

  let nonneg_non_det_ret : model = start_model @@ fun () -> assign_ret @= fresh_nonneg ()

  let ret_arg arg : model = start_model @@ fun () -> assign_ret (to_aval arg)

  let zero_or_minus_one_ret : model =
    start_model @@ fun () -> disj [assign_ret @= int (-1); assign_ret @= int 0]


  let non_det_or_minus_one_ret : model =
    start_model @@ fun () -> disj [assign_ret @= int (-1); assign_ret @= fresh ()]


  let null_or_non_det_ret =
    start_model @@ fun () -> disj [assign_ret @= null; assign_ret @= fresh ()]


  let null_or_nonneg_non_det_ret () : unit DSL.model_monad =
    disj [assign_ret @= null; assign_ret @= fresh_nonneg ()]


  let ret_alloc_or_null allocator =
    disj [assign_ret @= null; Basic.return_alloc_not_null allocator None ~initialize:true]


  let ret_alloc_or_minus_one allocator =
    disj [assign_ret @= int (-1); Basic.return_alloc_not_null allocator None ~initialize:true]


  let file_descriptor_allocator : Attribute.allocator DSL.model_monad =
    let* {callee_procname} = get_data in
    ret (Attribute.FileDescriptor callee_procname)


  let ret_fd_or_minus_one () = file_descriptor_allocator >>= ret_alloc_or_minus_one

  let ret_stream_or_null () = file_descriptor_allocator >>= ret_alloc_or_null

  let release_stream stream =
    let* {callee_procname} = get_data in
    Basic.free (FClose callee_procname) stream


  (* Unlike [free(NULL)], closing a NULL stream is undefined and crashes in common C libraries; only
     glibc and bionic make [closedir(NULL)] fail with [EINVAL], and glibc declares its argument
     nonnull. The NULL case of [Basic.free] must stay after [check_valid]: when [stream] is a
     parameter, it is the only precondition that a caller passing NULL matches, and hence how that
     caller gets reported. *)
  let close_stream stream = check_valid (FuncArg.arg_payload stream) @@> release_stream stream

  (* File descriptors are integers, not pointers: [0] is standard input, negative values are errors
     that calls reject with [EBADF], and constants such as [STDOUT_FILENO] are valid descriptors. *)
  let check_fd_not_closed fd = check_valid ~must_be_valid_reason:FileDescriptorUse fd

  let check_fd_not_released fd = check_valid ~must_be_valid_reason:FileDescriptorRelease fd

  (* negative values are not descriptors, and [0] is left valid as it is also the null pointer *)
  let release_fd invalidation fd =
    let fd_val = to_aval fd in
    disj
      [ prune_eq_zero fd_val
      ; ( prune_positive fd_val
        @@> let* {path; location} = get_data in
            invalidate path UntraceableAccess location invalidation (ValueOrigin.addr_hist fd) ) ]


  (** write [mk_value ()] into each scalar and pointer cell of an object of type [typ] at [addr],
      recursing into struct fields but skipping arrays *)
  let write_object_cells addr typ ~mk_value : unit DSL.model_monad =
    let* {path; analysis_data= {tenv}; location} = get_data in
    exec_command
      (AbductiveDomain.fold_pointer_targets tenv path (`Malloc addr) typ location
         ~f:(fun cell astate ->
           AbductiveDomain.Memory.add_edge path cell Dereference (mk_value ()) location astate ) )


  (* The cells written one by one all end up in the pre and post of the summaries of the callers,
     which gets costly for big objects copied through chains of callees. *)
  let max_cells_written_one_by_one = 64

  let has_few_cells tenv typ =
    let rec remaining budget (typ : Typ.t) =
      if budget < 0 then budget
      else
        match typ.desc with
        | Tint _ | Tfloat _ | Tptr _ ->
            budget - 1
        | Tstruct name -> (
          match Tenv.lookup tenv name with
          | None ->
              budget
          | Some {fields} ->
              List.fold fields ~init:budget ~f:(fun budget ({typ} : Struct.field) ->
                  remaining budget typ ) )
        | Tarray _ | Tvoid | Tfun _ | TVar _ ->
            budget
    in
    remaining max_cells_written_one_by_one typ >= 0


  (** write zero into each scalar and pointer cell of a new object of type [typ] at [addr],
      recursing into struct fields but skipping arrays, unions and struct fields at offset 0: Pulse
      does not relate the cells written through another member of a union, or through a pointer to
      the object cast to the type of its first field, to the ones of the object *)
  let rec zero_cells addr (typ : Typ.t) : unit DSL.model_monad =
    let* {analysis_data= {tenv}} = get_data in
    match typ.desc with
    | Tint _ | Tfloat _ | Tptr _ ->
        store ~ref:addr @= null
    | Tstruct name when Typ.Name.is_union name ->
        ret ()
    | Tstruct name -> (
      match Tenv.lookup tenv name with
      | None ->
          ret ()
      | Some {fields} ->
          let fields =
            match fields with
            | ({typ= {Typ.desc= Tstruct _}} : Struct.field) :: fields_after_first ->
                fields_after_first
            | _ ->
                fields
          in
          list_iter fields ~f:(fun {Struct.name= field; typ= field_typ} ->
              if Fieldname.is_internal field || Fieldname.is_capture_field_in_closure field then
                ret ()
              else
                let* field_addr = access NoAccess addr (FieldAccess field) in
                zero_cells field_addr field_typ ) )
    | Tarray _ | Tvoid | Tfun _ | TVar _ ->
        ret ()


  (** the object at [dest] gets unknown contents: the cells of the object already in the heap get
      fresh values, and so do the other cells when they are read later or seen by callers *)
  let overwrite_contents dest : unit DSL.model_monad =
    let* hist = add_model_call ValueHistory.epoch in
    exec_command (AbductiveDomain.overwrite_contents hist (fst dest))
    @@> store ~ref:dest @= fresh ()


  (** the first field of [name] when an object of type [typ] at the start of a [name] is exactly
      that field, or nested in it as its first field *)
  let rec first_field_holding tenv name typ =
    match Tenv.lookup tenv name with
    | Some {Struct.fields= ({name= field; typ= field_typ} : Struct.field) :: _} -> (
        if Typ.equal_ignore_quals field_typ typ then Some field
        else
          match field_typ.Typ.desc with
          | Tstruct field_typ_name ->
              first_field_holding tenv field_typ_name typ |> Option.map ~f:(fun _ -> field)
          | _ ->
              None )
    | _ ->
        None


  let rec has_pointer_cell tenv (typ : Typ.t) =
    match typ.desc with
    | Tptr _ ->
        true
    | Tstruct name ->
        Tenv.lookup tenv name
        |> Option.exists ~f:(fun {Struct.fields} ->
            List.exists fields ~f:(fun ({typ} : Struct.field) -> has_pointer_cell tenv typ) )
    | Tint _ | Tfloat _ | Tarray _ | Tvoid | Tfun _ | TVar _ ->
        false


  (** the objects written by [overwrite_cells], as their canonical address and their struct type
      name, or [None] for a scalar *)
  module Visited = Stdlib.Set.Make (struct
    type t = AbstractValue.t * Typ.Name.t option [@@deriving compare]
  end)

  (** write the values of the pointer cells of the object of type [typ] at [src] (fresh values if
      [src] is [None]) into the corresponding cells of [dest], and fresh values into its other
      scalar cells, recursing into struct fields. The cells already at [dest] that are not cells of
      the objects written so far, in [visited], are left alone when they are outside of the object,
      eg the fields after a first field of type [typ], and get unknown contents otherwise. Objects
      are written once: a struct and its first field can be at the same address in the heap when the
      path condition says so. *)
  let rec overwrite_cells ~src dest (typ : Typ.t) visited : Visited.t DSL.model_monad =
    let* {analysis_data= {tenv}} = get_data in
    let* addr =
      exec_pure_operation (fun astate ->
          (AbductiveDomain.CanonValue.canon' astate (fst dest) :> AbstractValue.t) )
    in
    let overwrite_cell visited =
      let* value =
        match (src, typ.desc) with
        | Some src, Tptr _ ->
            access NoAccess src Dereference
        | _ ->
            fresh ()
      in
      store ~ref:dest value @@> ret visited
    in
    let overwrite_field visited {Struct.name= field; typ= field_typ} =
      let* dest_field = access NoAccess dest (FieldAccess field) in
      let* src_field =
        match src with
        | Some src when has_pointer_cell tenv field_typ ->
            let* src_field = access NoAccess src (FieldAccess field) in
            ret (Some src_field)
        | _ ->
            ret None
      in
      overwrite_cells ~src:src_field dest_field field_typ visited
    in
    let is_written visited (access : Access.t) =
      match access with
      | FieldAccess field ->
          Visited.mem (addr, Some (Fieldname.get_class_name field)) visited
      | Dereference ->
          Visited.mem (addr, None) visited
      | ArrayAccess _ ->
          false
    in
    let overwrite_other_cells visited =
      let* edges =
        exec_pure_operation (fun astate ->
            AbductiveDomain.Memory.fold_edges addr astate ~init:[] ~f:(fun edges edge ->
                edge :: edges ) )
      in
      list_fold edges ~init:visited ~f:(fun visited ((access : Access.t), cell) ->
          if is_written visited access then ret visited
          else
            match access with
            | FieldAccess field -> (
              match first_field_holding tenv (Fieldname.get_class_name field) typ with
              | Some first_field when Fieldname.equal field first_field ->
                  overwrite_cells ~src cell typ visited
              | Some _ ->
                  ret visited
              | None ->
                  overwrite_contents cell @@> ret visited )
            | ArrayAccess _ ->
                overwrite_contents cell @@> ret visited
            | Dereference ->
                (store ~ref:dest @= fresh ()) @@> ret visited )
    in
    let visit name_opt ~write =
      if Visited.mem (addr, name_opt) visited then ret visited
      else write (Visited.add (addr, name_opt) visited) >>= overwrite_other_cells
    in
    match typ.desc with
    | Tint _ | Tfloat _ | Tptr _ ->
        visit None ~write:overwrite_cell
    | Tstruct name -> (
      match Tenv.lookup tenv name with
      | None ->
          overwrite_contents dest @@> ret visited
      | Some {fields} ->
          let fields =
            List.filter fields ~f:(fun ({name= field} : Struct.field) ->
                not (Fieldname.is_internal field || Fieldname.is_capture_field_in_closure field) )
          in
          visit (Some name) ~write:(fun visited ->
              list_fold fields ~init:visited ~f:overwrite_field ) )
    | Tarray _ | Tvoid | Tfun _ | TVar _ ->
        overwrite_contents dest @@> ret visited


  (** [Some T] when [size] is the size of exactly one [T] *)
  let single_object_type (size : Exp.t) =
    match Exp.ignore_cast size with
    | Sizeof {typ} ->
        Some typ
    | BinOp (Mult _, n, m) -> (
      match (Exp.ignore_cast n, Exp.ignore_cast m) with
      | (Const (Cint n), Sizeof {typ} | Sizeof {typ}, Const (Cint n)) when IntLit.isone n ->
          Some typ
      | _ ->
          None )
    | _ ->
        None


  (** the object at [dest] is overwritten, with the contents of the object at [src] if given; when
      the type of the object is known and it has few cells, they are written one by one *)
  let overwrite_object ?src dest typ_opt : unit DSL.model_monad =
    let* {analysis_data= {tenv}} = get_data in
    let dest = to_aval dest in
    match typ_opt with
    | Some typ when has_few_cells tenv typ ->
        let* (_ : Visited.t) =
          overwrite_cells ~src:(Option.map src ~f:to_aval) dest typ Visited.empty
        in
        ret ()
    | _ ->
        overwrite_contents dest


  let overwrite_pointee {FuncArg.arg_payload= ptr; typ} =
    let pointee_typ_opt =
      match typ.Typ.desc with Tptr (pointee_typ, _) -> Some pointee_typ | _ -> None
    in
    check_valid ptr @@> overwrite_object ptr pointee_typ_opt


  let calloc ~x nmemb size =
    start_model
    @@ fun () ->
    let total_size_exp = Exp.BinOp (Mult None, nmemb, size) in
    let alloc =
      let* block =
        lift_to_monad_and_get_result
          ( start_model
          @@ fun () -> Basic.return_alloc_not_null CMalloc ~initialize:true (Some total_size_exp) )
      in
      (* Pulse relates neither [p[0]] to [*p] nor [p[i].f] to [p->f], so zeros written in an array
         would survive writes through indices: only zero the cells of a single object, as [memset]
         does. *)
      let* {analysis_data= {tenv}} = get_data in
      option_iter
        (single_object_type total_size_exp |> Option.filter ~f:(has_few_cells tenv))
        ~f:(zero_cells block)
      @@> assign_ret block
    in
    if x then alloc else disj [alloc; return_null_dsl]


  let close fd : model =
    start_model
    @@ fun () ->
    check_fd_not_released fd
    @@> disj
          [ prune_lt_int (to_aval fd) IntLit.zero @@> assign_ret @= int (-1)
          ; (let* {callee_procname} = get_data in
             release_fd (FClose callee_procname) fd ) ]


  let fclose stream : model =
    start_model
    @@ fun () -> close_stream stream @@> disj [assign_ret @= int (-1 (* EOF *)); assign_ret @= int 0]


  let pclose stream : model =
    start_model
    @@ fun () -> close_stream stream @@> assign_ret (* exit status of the command *) @= fresh ()


  let closedir dirp : model =
    start_model
    @@ fun () ->
    close_stream dirp
    (* pretend [closedir] always succeeds, i.e. [dirp] was a valid stream descriptor or
       [close_stream] above would have caught an error *)
    @@> (int 0 >>= assign_ret)


  let confstr buf size =
    start_model
    @@ fun () ->
    let* () =
      disj
        [ prune_ne_zero (to_aval size) @@> check_valid buf @@> overwrite_object buf None
        ; prune_eq_zero (to_aval size) @@> prune_eq_zero (to_aval buf) ]
    in
    fresh_nonneg () >>= assign_ret


  let fgetpos stream pos =
    start_model
    @@ fun () ->
    check_valid stream @@> overwrite_pointee pos
    @@> disj [assign_ret @= int 0; assign_ret @= int (-1)]


  let getcwd buf _size : model =
    start_model
    @@ fun () ->
    disj
      [ prune_eq_zero (to_aval buf) @@> ret_alloc_or_null CMalloc
      ; prune_ne_zero (to_aval buf)
        @@> check_valid buf
        @@> disj [assign_ret @= null; assign_ret (to_aval buf)] ]


  let gets str : model =
    start_model @@ fun () -> check_valid str @@> disj [assign_ret @= null; assign_ret (to_aval str)]


  let fgets str stream : model =
    start_model
    @@ fun () ->
    check_valid stream @@> check_valid str
    @@> disj [assign_ret @= null; assign_ret (to_aval str)]
    @@> data_dependency str [str; stream]


  let eof_or_count_ret () : unit DSL.model_monad =
    let* res = fresh () in
    prune_ge_int res IntLit.minus_one @@> assign_ret res


  (* the format is not parsed: any pointer argument may receive input *)
  let write_scanf_outputs inputs (args : ValueOrigin.t FuncArg.t list) : unit DSL.model_monad =
    list_iter args ~f:(fun {FuncArg.arg_payload= arg; typ} ->
        if Typ.is_pointer typ then havoc_pointee (to_aval arg) @@> data_dependency arg inputs
        else ret () )


  let has_n_conversion format =
    let len = String.length format in
    let rec after_percent i =
      if i >= len then false
      else
        match format.[i] with
        | 'n' ->
            true
        | '*' | '$' | '\'' | '0' .. '9' | 'h' | 'j' | 'l' | 'L' | 'm' | 'q' | 't' | 'z' ->
            after_percent (i + 1)
        | _ ->
            scan (i + 1)
    and scan i =
      if i >= len then false
      else if Char.equal format.[i] '%' then after_percent (i + 1)
      else scan (i + 1)
    in
    scan 0


  (* The result counts the assigned conversions, each of which consumes an argument. Nothing is
     assigned when it is 0 or EOF, except by [%n], which is not counted. *)
  let scanf_outputs_and_ret format inputs args : unit DSL.model_monad =
    let ret_between low high =
      let* res = fresh () in
      prune_ge_int res (IntLit.of_int low)
      @@> prune_lt_int res (IntLit.of_int (high + 1))
      @@> assign_ret res
    in
    let num_args = List.length args in
    let* format_str = as_constant_string (to_aval format) in
    match format_str with
    | Some format_str when not (has_n_conversion format_str) ->
        disj [ret_between (-1) 0; write_scanf_outputs inputs args @@> ret_between 1 num_args]
    | _ ->
        write_scanf_outputs inputs args @@> ret_between (-1) num_args


  let fscanf input format args : model =
    start_model
    @@ fun () ->
    check_valid input @@> check_valid format @@> scanf_outputs_and_ret format [input] args


  let scanf format args : model =
    start_model @@ fun () -> check_valid format @@> scanf_outputs_and_ret format [] args


  (* the outputs are behind the [va_list] argument, which is not modelled *)
  let vfscanf input format : model =
    start_model @@ fun () -> check_valid input @@> check_valid format @@> eof_or_count_ret ()


  let vscanf format : model = start_model @@ fun () -> check_valid format @@> eof_or_count_ret ()

  (* A NULL [*lineptr] gets a new buffer, which glibc, musl and macOS allocate even when the call
     fails. A non-NULL one may be reallocated, so it is not tracked anymore. *)
  let getdelim lineptr n stream : model =
    let lineptr = to_aval lineptr in
    start_model
    @@ fun () ->
    let* () = check_valid stream in
    let* old_buf = load lineptr in
    let new_buf =
      lift_to_monad_and_get_result
        (start_model @@ fun () -> Basic.return_alloc_not_null CMalloc None ~initialize:true)
    in
    let reallocated_buf =
      let* () = apply_unknown_effect old_buf in
      let* buf = fresh () in
      and_positive buf @@> ret buf
    in
    let* buf =
      disj [prune_eq_zero old_buf @@> new_buf; prune_ne_zero old_buf @@> reallocated_buf]
    in
    store ~ref:lineptr buf
    @@> (store ~ref:(to_aval n) @= fresh_nonneg ())
    @@> data_dependency (ValueOrigin.unknown buf) [stream]
    @@> eof_or_count_ret ()


  (* the stream may use [buf] until it is closed *)
  let setbuf stream buf : model =
    start_model @@ fun () -> check_valid stream @@> apply_unknown_effect (to_aval buf)


  let memcpy dest src size : model =
    start_model
    @@ fun () ->
    check_valid dest @@> check_valid src
    @@> overwrite_object ~src dest (single_object_type size)
    @@> data_dependency dest [src]
    @@> assign_ret (to_aval dest)


  let fill_object s size value : unit DSL.model_monad =
    check_valid s
    @@> option_iter (single_object_type size) ~f:(fun typ ->
        write_object_cells (to_aval s) typ ~mk_value:(fun () -> value) )


  let memset s value size : model =
    start_model @@ fun () -> fill_object s size (to_aval value) @@> assign_ret (to_aval s)


  let bzero s size : model = start_model @@ fun () -> fill_object s size @= null

  let open_ = start_model ret_fd_or_minus_one

  let dup fd : model = start_model @@ fun () -> check_fd_not_closed fd @@> ret_fd_or_minus_one ()

  let unknown_call args =
    PulseModelsImport.Basic.skipped_known_call (ValueOrigin.addr_hist_args args)
    |> lift_model |> lift_to_monad


  (* treat the arguments like those of an unknown call so that [addr] and [addrlen], which receive
     the address of the peer, get havocked *)
  let accept sockfd args : model =
    start_model
    @@ fun () ->
    check_fd_not_closed (FuncArg.arg_payload sockfd)
    @@> unknown_call (sockfd :: args)
    @@> ret_fd_or_minus_one ()


  (* apart from the check of [fd], the call is treated as unknown, e.g. to havoc the buffers that it
     writes to *)
  let use_fd fd args : model =
    start_model
    @@ fun () -> check_fd_not_closed (FuncArg.arg_payload fd) @@> unknown_call (fd :: args)


  (* for [pipe], [pipe2] and [socketpair] *)
  let fd_pair fds : model =
    start_model
    @@ fun () ->
    (* descriptors stored through the result of pointer arithmetic such as [fds + 2 * i], or through
       a pointer returned by an unknown function such as [std::array::data()], would be unreachable
       for Pulse, i.e. leaked, right away *)
    let* returned_from_unknown =
      AddressAttributes.get_valid_returned_from_unknown (ValueOrigin.value fds)
      |> exec_pure_operation
    in
    let track_fds =
      match (fds : ValueOrigin.t) with
      | Unknown _ ->
          false
      | InMemory _ | OnStack _ ->
          Option.is_none returned_from_unknown
    in
    let new_fd_at index =
      let* index = int index in
      let* cell = access NoAccess (to_aval fds) (ArrayAccess (StdTyp.int, fst index)) in
      let* fd = fresh () in
      let* () =
        if track_fds then
          let* allocator = file_descriptor_allocator in
          allocation allocator fd
        else ret ()
      in
      and_positive fd @@> store ~ref:cell fd
    in
    disj [assign_ret @= int (-1); new_fd_at 0 @@> new_fd_at 1 @@> assign_ret @= int 0]


  let fopen path mode : model =
    start_model
    @@ fun () ->
    check_valid path @@> check_valid mode @@> ret_stream_or_null () @@> data_dependency_to_ret [path]


  let fprintf stream format args =
    start_model
    @@ fun () ->
    check_valid stream @@> check_valid format
    @@> data_dependency stream (format :: args)
    @@> assign_ret (* pretend [fprintf] always succeeds *) @= fresh_nonneg ()


  let sprintf str format args =
    start_model
    @@ fun () ->
    check_valid str @@> check_valid format
    @@> data_dependency str (format :: args)
    @@> assign_ret (* pretend [snprintf] always succeeds *) @= fresh_nonneg ()


  let fputs s stream =
    start_model
    @@ fun () ->
    check_valid stream @@> check_valid s @@> data_dependency stream [s] @@> assign_ret
    (* pretend [fputs] always succeeds *) @= fresh_nonneg ()


  (* the new stream owns [fd], which stays open, but a failure leaves [fd] to the caller *)
  let stream_of_fd fd =
    check_fd_not_released fd
    @@> disj
          [ assign_ret @= null
          ; ( (let* {callee_procname} = get_data in
               release_fd (HandedOverToStream callee_procname) fd )
            @@> let* allocator = file_descriptor_allocator in
                Basic.return_alloc_not_null allocator None ~initialize:true ) ]


  let fdopen fd mode : model = start_model @@ fun () -> check_valid mode @@> stream_of_fd fd

  let fdopendir fd : model = start_model @@ fun () -> stream_of_fd fd

  let opendir path : model = start_model @@ fun () -> check_valid path @@> ret_stream_or_null ()

  let tmpfile : model = start_model ret_stream_or_null

  let putc c stream : model =
    start_model
    @@ fun () ->
    check_valid stream
    @@> disj [assign_ret @= int (-1); assign_ret (to_aval c)]
    @@> data_dependency stream [c]


  let read_common fd buf count size_exp =
    disj
      [ prune_ne_zero (to_aval count)
        @@> check_valid buf
        @@> overwrite_object buf (single_object_type size_exp)
        @@> data_dependency buf [fd] @@> assign_ret @= fresh_nonneg ()
      ; prune_eq_zero (to_aval count) @@> assign_ret @= int 0 ]


  let read_fd fd buf {FuncArg.arg_payload= count; exp= count_exp} =
    start_model @@ fun () -> check_fd_not_closed fd @@> read_common fd buf count count_exp


  let fread ptr size {FuncArg.arg_payload= nmemb; exp= nmemb_exp} stream =
    start_model
    @@ fun () ->
    check_valid stream @@> read_common stream ptr nmemb (Exp.BinOp (Mult None, size, nmemb_exp))


  let shmget : model = start_model @@ fun () -> ret_alloc_or_minus_one CMalloc

  let statfs path buf =
    start_model
    @@ fun () ->
    check_valid path @@> overwrite_pointee buf @@> disj [assign_ret @= int 0; assign_ret @= int (-1)]


  let stpcpy dst src : model =
    start_model
    @@ fun () ->
    (store ~ref:(to_aval dst) @= fresh_nonneg ())
    @@> check_valid src @@> data_dependency dst [src] @@> assign_ret @= fresh_nonneg ()


  let strchr str _c : model =
    start_model @@ fun () -> check_valid str @@> null_or_nonneg_non_det_ret ()


  let strcpy dst src : model =
    start_model
    @@ fun () ->
    (store ~ref:(to_aval dst) @= fresh_nonneg ())
    @@> check_valid src @@> data_dependency dst [src]
    @@> assign_ret (to_aval dst)


  let strdup str : model =
    start_model
    @@ fun () ->
    check_valid str
    @@> alloc_common_dsl ~null_case:true ~initialize:true CMalloc None
    @@> data_dependency_to_ret [str]


  let strpbrk str accept : model =
    start_model
    @@ fun () -> check_valid str @@> check_valid accept @@> null_or_nonneg_non_det_ret ()


  let strstr haystack needle : model =
    start_model
    @@ fun () -> check_valid haystack @@> check_valid needle @@> null_or_nonneg_non_det_ret ()


  (* [*endptr] gets a fresh non-NULL pointer rather than [str] itself, which would make the usual
     [end == str] check (nothing parsed) always true and prune the success path. *)
  let strtol str endptr : model =
    start_model
    @@ fun () ->
    let* () = check_valid str in
    let* end_ = fresh () in
    let* () =
      disj
        [ prune_eq_zero (to_aval endptr)
        ; prune_ne_zero (to_aval endptr)
          @@> and_positive end_
          @@> store ~ref:(to_aval endptr) end_
          @@> data_dependency (ValueOrigin.unknown end_) [str] ]
    in
    (assign_ret @= fresh ()) @@> data_dependency_to_ret [str]


  let time tloc =
    start_model
    @@ fun () ->
    let* t = fresh () in
    let* () =
      disj
        [prune_eq_zero (to_aval tloc); prune_ne_zero (to_aval tloc) @@> store ~ref:(to_aval tloc) t]
    in
    assign_ret t


  let write_model fd buf count =
    start_model
    @@ fun () ->
    disj
      [ prune_ne_zero (to_aval count)
        @@> check_valid buf @@> data_dependency fd [buf] @@> assign_ret @= fresh_nonneg ()
      ; prune_eq_zero (to_aval count) @@> assign_ret @= int 0 ]


  let write_fd fd buf count =
    start_model @@ fun () -> check_fd_not_closed fd @@> (write_model fd buf count |> lift_to_monad)


  let fwrite ptr size stream =
    start_model @@ fun () -> check_valid stream @@> (write_model stream ptr size |> lift_to_monad)


  let assertion_error _ : model = start_model @@ fun () -> report_assert_error

  let unreachable_path _ : model = start_model @@ fun () -> unreachable
end

(** Reference: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html

    Model these atomic operations as just their regular, non-concurrency-aware version. *)
module Atomic = struct
  open DSL.Syntax

  let atomic_load_n ptr = start_model @@ fun () -> load (to_aval ptr) >>= assign_ret

  let atomic_load ptr ret = start_model @@ fun () -> load (to_aval ptr) >>= store ~ref:(to_aval ret)

  let atomic_store_n ref obj = start_model @@ fun () -> store ~ref:(to_aval ref) (to_aval obj)

  let atomic_store ref obj =
    start_model @@ fun () -> load (to_aval obj) >>= store ~ref:(to_aval ref)


  (** This built-in function implements an atomic exchange operation. It writes val into *ptr, and
      returns the previous contents of *ptr. *)
  let atomic_exchange_n ptr val_ =
    let ptr = to_aval ptr in
    let val_ = to_aval val_ in
    start_model
    @@ fun () ->
    let* prev_ptr = load ptr in
    store ~ref:ptr val_ @@> assign_ret prev_ptr


  (** This is the generic version of an atomic exchange. It stores the contents of *val into *ptr.
      The original value of *ptr is copied into *ret. *)
  let atomic_exchange ptr val_ ret =
    let ptr = to_aval ptr in
    let val_ = to_aval val_ in
    let ret = to_aval ret in
    start_model
    @@ fun () ->
    let* prev_ptr = load ptr in
    (store ~ref:ptr @= load val_) @@> store ~ref:ret prev_ptr


  (** pretend this always succeeds since we don't model parallelism *)
  let atomic_compare_exchange_n ptr expected desired =
    let ptr = to_aval ptr in
    let desired = to_aval desired in
    start_model @@ fun () -> check_valid expected @@> store ~ref:ptr desired @@> assign_ret @= int 1


  (** The function is virtually identical to __atomic_compare_exchange_n, except the desired value
      is also a pointer. *)
  let atomic_compare_exchange ptr expected desired =
    let ptr = to_aval ptr in
    let desired = to_aval desired in
    start_model
    @@ fun () -> check_valid expected @@> (store ~ref:ptr @= load desired) @@> assign_ret @= int 1


  let do_atomic_op op v1 v2 =
    match op with
    | `Add ->
        binop (PlusA None) v1 v2
    | `Sub ->
        binop (MinusA None) v1 v2
    | `And ->
        binop BAnd v1 v2
    | `Xor ->
        binop BXor v1 v2
    | `Or ->
        binop BOr v1 v2
    | `Nand ->
        unop Neg @= binop BAnd v1 v2


  let atomic_op_fetch pre_or_post op ptr val_ =
    let ptr = to_aval ptr in
    let val_ = to_aval val_ in
    start_model
    @@ fun () ->
    let* pre = load ptr in
    let* result = do_atomic_op op pre val_ in
    store ~ref:ptr result @@> assign_ret @@ match pre_or_post with `Pre -> pre | `Post -> result


  (** This built-in function performs an atomic test-and-set operation on the byte at *ptr. The byte
      is set to some implementation defined nonzero "set" value and the return value is true if and
      only if the previous contents were "set". It should be only used for operands of type bool or
      char. For other types only part of the value may be set. *)
  let atomic_test_and_set ptr =
    let ptr = to_aval ptr in
    start_model
    @@ fun () ->
    let* set = fresh () in
    let* () = prune_gt set @= int 0 in
    store ~ref:ptr set


  (** This built-in function performs an atomic clear operation on *ptr. After the operation, *ptr
      contains 0. *)
  let atomic_clear ptr =
    let ptr = to_aval ptr in
    start_model @@ fun () -> store ~ref:ptr @= int 0
end

module Glib = struct
  open DSL.Syntax

  let g_malloc size =
    start_model
    @@ fun () ->
    disj
      [ prune_eq_zero (to_aval @@ FuncArg.arg_payload size) @@> assign_ret @= null
      ; prune_ne_zero (to_aval @@ FuncArg.arg_payload size)
        @@> lift_to_monad
        @@ alloc_common ~initialize:false ~null_case:false ~desc:"g_malloc" CMalloc
             (Some (FuncArg.exp size)) ]


  let g_realloc pointer size =
    start_model
    @@ fun () ->
    disj
      [ prune_eq_zero (to_aval @@ FuncArg.arg_payload size) @@> assign_ret @= null
      ; prune_ne_zero (to_aval @@ FuncArg.arg_payload size)
        @@> lift_to_monad
        @@ realloc_common ~null_case:false ~desc:"g_realloc" CMalloc pointer (FuncArg.exp size) ]
end

module Xlib = struct
  open DSL.Syntax

  let xGetAtomName =
    alloc_common ~null_case:true ~initialize:false ~desc:"XGetAtomName" CMalloc None


  let xFree pointer =
    start_model
    @@ fun () ->
    prune_ne_zero (to_aval @@ FuncArg.arg_payload pointer) @@> lift_to_monad (free pointer)
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let open DSL.Syntax in
  let int_ptr_typ = Typ.mk_ptr StdTyp.int in
  (* user code may define functions named like the getline family with other signatures; [size_t]
     is [unsigned long] on LP64 targets and [unsigned int] on ILP32 ones *)
  let getline_family size_t =
    let char_ptr_ptr = Typ.mk_ptr (Typ.mk_ptr StdTyp.char) in
    let size_t_ptr = Typ.mk_ptr (Typ.mk (Tint size_t)) in
    [ -"getdelim"
      <>$ capt_arg_payload_of_prim_typ char_ptr_ptr
      $+ capt_arg_payload_of_prim_typ size_t_ptr
      $+ any_arg_of_prim_typ StdTyp.int $+ capt_arg_payload $--> getdelim
    ; -"getline"
      <>$ capt_arg_payload_of_prim_typ char_ptr_ptr
      $+ capt_arg_payload_of_prim_typ size_t_ptr
      $+ capt_arg_payload $--> getdelim ]
  in
  let match_regexp_opt r_opt (_tenv, proc_name) _ =
    Option.exists r_opt ~f:(fun r ->
        let s = Procname.to_string proc_name in
        Str.string_match r s 0 )
  in
  let rev_compose1 model2 model1 = compose1 model1 model2 in
  let ignore_arg model _arg = model in
  let ignore_args2 model _arg1 _arg2 = model in
  let taint_ret_from_arg arg = start_model @@ fun () -> data_dependency_to_ret [arg] in
  let map_context_tenv f (x, _) = f x in
  [ +BuiltinDecl.(match_builtin free) <>$ capt_arg $--> free
  ; +match_regexp_opt Config.pulse_model_free_pattern <>$ capt_arg $+...$--> free
  ; -"realloc" <>$ capt_arg $+ capt_exp $--> realloc ~null_case:true
  ; +match_regexp_opt Config.pulse_model_realloc_pattern
    <>$ capt_arg $+ capt_exp $+...$--> custom_realloc ~null_case:true
  ; +BuiltinDecl.(match_builtin __call_c_function_ptr) $ capt_arg $++$--> call_c_function_ptr
  ; ( -"accept" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg
    $--> fun sockfd addr addrlen -> accept sockfd [addr; addrlen] )
  ; ( -"accept4" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg
    $--> fun sockfd addr addrlen flags -> accept sockfd [addr; addrlen; flags] )
  ; -"access" <>$ capt_arg_payload $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"android_fdsan_close_with_tag"
    <>$ capt_arg_payload_of_prim_typ StdTyp.int
    $+ any_arg $--> close
  ; -"asctime" <>$ capt_arg_payload
    $--> compose1 (ignore_arg @@ start_model @@ null_or_nonneg_non_det_ret) taint_ret_from_arg
  ; ( -"__assert_fail" <>$ capt_arg
    $--> if Config.pulse_report_assert then assertion_error else unreachable_path )
  ; -"__atomic_load_n" <>$ capt_arg_payload $+ any_arg $--> Atomic.atomic_load_n
  ; -"__atomic_load" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> Atomic.atomic_load
  ; -"__atomic_store_n" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_store_n
  ; -"__atomic_store" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> Atomic.atomic_store
  ; -"__atomic_exchange_n" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_exchange_n
  ; -"__atomic_exchange" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_exchange
  ; -"__atomic_compare_exchange" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ any_arg $+ any_arg $+ any_arg $--> Atomic.atomic_compare_exchange
  ; -"__atomic_compare_exchange_n" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ any_arg $+ any_arg $+ any_arg $--> Atomic.atomic_compare_exchange_n
  ; -"__atomic_add_fetch" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Post `Add
  ; -"__atomic_sub_fetch" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Post `Sub
  ; -"__atomic_and_fetch" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Post `And
  ; -"__atomic_xor_fetch" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Post `Xor
  ; -"__atomic_or_fetch" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Post `Or
  ; -"__atomic_nand_fetch" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Post `Nand
  ; -"__atomic_fetch_add" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Pre `Add
  ; -"__atomic_fetch_sub" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Pre `Sub
  ; -"__atomic_fetch_and" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Pre `And
  ; -"__atomic_fetch_xor" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Pre `Xor
  ; -"__atomic_fetch_or" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Pre `Or
  ; -"__atomic_fetch_nand" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> Atomic.atomic_op_fetch `Pre `Nand
  ; -"__atomic_test_and_set" <>$ capt_arg_payload $+ any_arg $--> Atomic.atomic_test_and_set
  ; -"__atomic_clear" <>$ capt_arg_payload $+ any_arg $--> Atomic.atomic_clear
  ; -"__builtin_memset" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_exp $--> memset
  ; -"bzero" <>$ capt_arg_payload $+ capt_exp $--> bzero
  ; -"clearerr" <>$ capt_arg_payload $--> valid_arg
  ; -"close" <>$ capt_arg_payload_of_prim_typ StdTyp.int $--> close
  ; -"closedir" <>$ capt_arg $--> closedir
  ; -"confstr" <>$ any_arg $+ capt_arg_payload $+ capt_arg_payload $--> confstr
  ; -"creat" <>$ any_arg $+ any_arg $--> open_
  ; -"creat64" <>$ any_arg $+ any_arg $--> open_
  ; -"ctime" <>$ capt_arg_payload
    $--> compose1 (ignore_arg @@ start_model @@ null_or_nonneg_non_det_ret) taint_ret_from_arg
  ; -"dup" <>$ capt_arg_payload_of_prim_typ StdTyp.int $--> dup
  ; -"epoll_create" <>$ any_arg $--> open_
  ; -"epoll_create1" <>$ any_arg $--> open_
  ; -"eventfd" <>$ any_arg $+ any_arg $--> open_
  ; -"explicit_bzero" <>$ capt_arg_payload $+ capt_exp $--> bzero
  ; -"fclose" <>$ capt_arg $--> fclose
  ; ( -"fcntl" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg
    $++$--> fun fd cmd args -> use_fd fd (cmd :: args) )
  ; (-"fdatasync" <>$ capt_arg_of_prim_typ StdTyp.int $--> fun fd -> use_fd fd [])
  ; -"fdopen" <>$ capt_arg_payload_of_prim_typ StdTyp.int $+ capt_arg_payload $--> fdopen
  ; -"fdopendir" <>$ capt_arg_payload_of_prim_typ StdTyp.int $--> fdopendir
  ; -"feof" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"ferror" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"fflush" <>$ capt_arg_payload $--> compose1 null_or_valid_arg (ignore_arg non_det_ret)
  ; -"fgetc" <>$ capt_arg_payload
    $--> (valid_arg |> rev_compose1 (ignore_arg non_det_ret) |> rev_compose1 taint_ret_from_arg)
  ; -"fgetpos" <>$ capt_arg_payload $+ capt_arg $--> fgetpos
  ; -"fgets" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $--> fgets
  ; -"fileno" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"fopen" <>$ capt_arg_payload $+ capt_arg_payload $--> fopen
  ; -"fprintf" <>$ capt_arg_payload $+ capt_arg_payload $+++$--> fprintf
  ; -"fputc" <>$ capt_arg_payload $+ capt_arg_payload $--> putc
  ; -"fputs" <>$ capt_arg_payload $+ capt_arg_payload $--> fputs
  ; -"fread" <>$ capt_arg_payload $+ capt_exp $+ capt_arg $+ capt_arg_payload $--> fread
  ; -"fscanf" <>$ capt_arg_payload $+ capt_arg_payload $++$--> fscanf
  ; -"fsctl" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $+ any_arg
    $--> compose2 valid_and_null_or_valid_args (ignore_args2 zero_or_minus_one_ret)
  ; -"fseek" <>$ capt_arg_payload $+ any_arg $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fseeko" <>$ capt_arg_payload $+ any_arg $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fseeko64" <>$ capt_arg_payload $+ any_arg $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fsetpos" <>$ capt_arg_payload $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"fsetpos" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 zero_or_minus_one_ret)
  ; (-"fstat" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $--> fun fd buf -> use_fd fd [buf])
  ; (-"fsync" <>$ capt_arg_of_prim_typ StdTyp.int $--> fun fd -> use_fd fd [])
  ; -"ftell" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_or_minus_one_ret)
  ; -"ftello" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_or_minus_one_ret)
  ; -"ftello64" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_or_minus_one_ret)
  ; ( -"ftruncate" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg
    $--> fun fd length -> use_fd fd [length] )
  ; -"fwrite" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $+ capt_arg_payload $--> fwrite
  ; -"g_free" <>$ capt_arg $--> free
  ; -"g_malloc" <>$ capt_arg $--> Glib.g_malloc
  ; -"g_realloc" <>$ capt_arg $+ capt_arg $--> Glib.g_realloc
  ; -"getc" <>$ capt_arg_payload
    $--> (valid_arg |> rev_compose1 (ignore_arg non_det_ret) |> rev_compose1 taint_ret_from_arg)
  ; -"getcwd" <>$ capt_arg_payload $+ capt_arg_payload $--> getcwd
  ; -"getenv" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg null_or_non_det_ret)
  ; -"getlogin" $$--> start_model @@ null_or_nonneg_non_det_ret
  ; -"getpwent" $$--> start_model @@ null_or_nonneg_non_det_ret
  ; -"getpwnam" <>$ any_arg $--> start_model @@ null_or_nonneg_non_det_ret
  ; -"getpwuid" <>$ any_arg $--> start_model @@ null_or_nonneg_non_det_ret
  ; -"gets" <>$ capt_arg_payload $--> gets
  ; -"gmtime" <>$ any_arg $--> start_model @@ null_or_nonneg_non_det_ret
  ; -"gtk_type_check_object_cast" <>$ capt_arg_payload $+ any_arg $--> assume_not_null
  ; -"gzdopen" <>$ capt_arg_payload_of_prim_typ StdTyp.int $+ capt_arg_payload $--> fdopen
  ; -"inotify_init" $$--> open_
  ; -"inotify_init1" <>$ any_arg $--> open_
  ; ( -"ioctl" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg
    $++$--> fun fd request args -> use_fd fd (request :: args) )
  ; -"localtime" <>$ any_arg $--> start_model @@ null_or_nonneg_non_det_ret
  ; -"longjmp" <>$ any_arg $+ any_arg $--> Basic.early_exit
  ; ( -"lseek" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg
    $--> fun fd offset whence -> use_fd fd [offset; whence] )
  ; -"memchr" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strchr
  ; -"memcmp" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> compose2 valid_args2 (ignore_args2 non_det_ret)
  ; -"memcpy" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_exp $--> memcpy
  ; -"memfd_create" <>$ any_arg $+ any_arg $--> open_
  ; -"memmove" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_exp $--> memcpy
  ; -"memrchr" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strchr
  ; -"memset" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_exp $--> memset
  ; -"mkostemp" <>$ any_arg $+ any_arg $--> open_
  ; -"mkstemp" <>$ any_arg $--> open_
  ; -"open" <>$ any_arg $+ any_arg $+? any_arg $--> open_
  ; -"open64" <>$ any_arg $+ any_arg $+? any_arg $--> open_
  ; -"openat" <>$ any_arg $+ any_arg $+ any_arg $+? any_arg $--> open_
  ; -"openat64" <>$ any_arg $+ any_arg $+ any_arg $+? any_arg $--> open_
  ; -"opendir" <>$ capt_arg_payload $--> opendir
  ; (-"pause" $$--> start_model @@ fun () -> assign_ret @= int (-1))
  ; -"pclose" <>$ capt_arg $--> pclose
  ; -"pipe" <>$ capt_arg_payload_of_prim_typ int_ptr_typ $--> fd_pair
  ; -"pipe2" <>$ capt_arg_payload_of_prim_typ int_ptr_typ $+ any_arg $--> fd_pair
  ; -"popen" <>$ capt_arg_payload $+ capt_arg_payload $--> fopen
  ; ( -"pread" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg
    $--> fun fd buf count offset -> use_fd fd [buf; count; offset] )
  ; (-"printf" &--> start_model @@ fun () -> assign_ret @= fresh ())
  ; -"pthread_exit" <>$ any_arg $+ any_arg $--> Basic.early_exit
  ; -"pthread_once" <>$ any_arg $+ capt_arg $--> pthread_once
  ; -"putc" <>$ capt_arg_payload $+ capt_arg_payload $--> putc
  ; -"puts" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; ( -"pwrite" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg
    $--> fun fd buf count offset -> use_fd fd [buf; count; offset] )
  ; -"read" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg $--> read_fd
  ; -"readdir" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg null_or_non_det_ret)
  ; -"readline" <>$ capt_arg_payload
    $--> compose1 null_or_valid_arg (ignore_arg null_or_non_det_ret)
  ; ( -"readv" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg
    $--> fun fd iov iovcnt -> use_fd fd [iov; iovcnt] )
  ; ( -"recv" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg
    $--> fun fd buf len flags -> use_fd fd [buf; len; flags] )
  ; ( -"recvfrom" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg
    $+ capt_arg $+ capt_arg
    $--> fun fd buf len flags addr addrlen -> use_fd fd [buf; len; flags; addr; addrlen] )
  ; ( -"recvmsg" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg
    $--> fun fd msg flags -> use_fd fd [msg; flags] )
  ; -"remove" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"rename" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 zero_or_minus_one_ret)
  ; -"rewind" <>$ capt_arg_payload $--> valid_arg
  ; -"scanf" <>$ capt_arg_payload $++$--> scanf
  ; ( -"send" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg
    $--> fun fd buf len flags -> use_fd fd [buf; len; flags] )
  ; ( -"sendmsg" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg
    $--> fun fd msg flags -> use_fd fd [msg; flags] )
  ; ( -"sendto" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg $+ capt_arg $+ capt_arg
    $+ capt_arg
    $--> fun fd buf len flags addr addrlen -> use_fd fd [buf; len; flags; addr; addrlen] )
  ; -"setbuf" <>$ capt_arg_payload $+ capt_arg_payload $--> setbuf
  ; -"setlocale" <>$ any_arg $+ capt_arg_payload
    $--> compose1 null_or_valid_arg (ignore_arg null_or_non_det_ret)
  ; -"setvbuf" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $+ any_arg
    $--> compose2 setbuf (ignore_args2 non_det_ret)
  ; -"shmget" <>$ any_arg $+ any_arg $+ any_arg $--> shmget
  ; -"snprintf" <>$ capt_arg_payload $+ any_arg (* size *) $+ capt_arg_payload $+++$--> sprintf
  ; -"socket" <>$ any_arg $+ any_arg $+ any_arg $--> open_
  ; -"socketpair" <>$ any_arg $+ any_arg $+ any_arg
    $+ capt_arg_payload_of_prim_typ int_ptr_typ
    $--> fd_pair
  ; -"sprintf" <>$ capt_arg_payload $+ capt_arg_payload $+++$--> sprintf
  ; -"sscanf" <>$ capt_arg_payload $+ capt_arg_payload $++$--> fscanf
  ; -"stat" <>$ capt_arg_payload $+ capt_arg $--> statfs
  ; -"statfs" <>$ capt_arg_payload $+ capt_arg $--> statfs
  ; -"stpcpy" <>$ capt_arg_payload $+ capt_arg_payload $--> stpcpy
  ; -"strcasestr" <>$ capt_arg_payload $+ capt_arg_payload $--> strstr
  ; -"strcat" <>$ capt_arg_payload $+ capt_arg_payload $--> strcpy
  ; -"strchr" <>$ capt_arg_payload $+ capt_arg_payload $--> strchr
  ; -"strcmp" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 non_det_ret)
  ; -"strcpy" <>$ capt_arg_payload $+ capt_arg_payload $--> strcpy
  ; -"strspn" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 non_det_ret)
  ; -"strcspn" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 nonneg_non_det_ret)
  ; -"strdup" <>$ capt_arg_payload $--> strdup
  ; -"strecpy" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $--> strcpy
  ; -"strlcat" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strcpy
  ; -"strlcpy" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strcpy
  ; -"strlen" <>$ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"strlwr" <>$ capt_arg_payload $--> compose1 valid_arg ret_arg
  ; -"strncat" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strcpy
  ; -"strncmp" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
    $--> compose2 valid_args2 (ignore_args2 non_det_ret)
  ; -"strncpy" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strcpy
  ; -"strpbrk" <>$ capt_arg_payload $+ capt_arg_payload $--> strpbrk
  ; -"strrchr" <>$ capt_arg_payload $+ capt_arg_payload $--> strchr
  ; -"strspn" <>$ capt_arg_payload $+ capt_arg_payload
    $--> compose2 valid_args2 (ignore_args2 non_det_ret)
  ; -"strstr" <>$ capt_arg_payload $+ capt_arg_payload $--> strstr
  ; -"strtcpy" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strcpy
  ; -"strtod" <>$ capt_arg_payload $+ capt_arg_payload $--> strtol
  ; -"strtol" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strtol
  ; -"strtoul" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> strtol
  ; -"strupr" <>$ capt_arg_payload $--> compose1 valid_arg ret_arg
  ; -"time" <>$ capt_arg_payload $--> time
  ; -"timerfd_create" <>$ any_arg $+ any_arg $--> open_
  ; -"tmpfile" $$--> tmpfile
  ; -"ungetc" <>$ any_arg $+ capt_arg_payload $--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"unlink" <>$ capt_arg_payload $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"utimes" <>$ capt_arg_payload $+ any_arg
    $--> compose1 valid_arg (ignore_arg zero_or_minus_one_ret)
  ; -"vfprintf" <>$ capt_arg_payload $+ capt_arg_payload $+++$--> fprintf
  ; -"vfscanf" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> vfscanf
  ; -"vprintf" <>$ capt_arg_payload $+...$--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"vscanf" <>$ capt_arg_payload $+ any_arg $--> vscanf
  ; -"vsnprintf" <>$ capt_arg_payload $+...$--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"vsprintf" <>$ capt_arg_payload $+...$--> compose1 valid_arg (ignore_arg non_det_ret)
  ; -"vsscanf" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg $--> vfscanf
  ; -"write" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> write_fd
  ; ( -"writev" <>$ capt_arg_of_prim_typ StdTyp.int $+ capt_arg $+ capt_arg
    $--> fun fd iov iovcnt -> use_fd fd [iov; iovcnt] )
  ; -"XGetAtomName" <>$ any_arg $+ any_arg $--> Xlib.xGetAtomName
  ; -"XFree" <>$ capt_arg $--> Xlib.xFree ]
  @ getline_family IULong @ getline_family IUInt
  @ ( [ +BuiltinDecl.(match_builtin malloc)
        <>$ capt_exp
        $--> malloc ~null_case:(not Config.pulse_unsafe_malloc)
      ; -"calloc" <>$ capt_exp $+ capt_exp $--> calloc ~x:false
      ; -"xmalloc" <>$ capt_exp $--> malloc ~null_case:false
      ; -"xcalloc" <>$ capt_exp $+ capt_exp $--> calloc ~x:true
      ; +match_regexp_opt Config.pulse_model_malloc_pattern
        <>$ capt_exp
        $+...$--> custom_malloc ~null_case:(not Config.pulse_unsafe_malloc)
      ; +map_context_tenv PatternMatch.ObjectiveC.is_core_graphics_create_or_copy
        &--> custom_alloc_not_null "CGCreate/Copy"
      ; +map_context_tenv PatternMatch.ObjectiveC.is_core_foundation_create_or_copy
        &--> custom_alloc_not_null "CFCreate/Copy"
      ; +BuiltinDecl.(match_builtin malloc_no_fail) <>$ capt_exp $--> malloc ~null_case:false
      ; +match_regexp_opt Config.pulse_model_alloc_pattern &--> custom_alloc_not_null "custom alloc"
      ]
    |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist) )
