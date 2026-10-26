(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type key =
  | Global of (Mangled.t * Typ.template_spec_info * SourceFile.t option)
  | Field of Fieldname.t
[@@deriving compare]

module KeySet = Stdlib.Set.Make (struct
  type t = key [@@deriving compare]
end)

(** the mutexes initialised as recursive in a procedure, and its formals among them *)
type scan = {keys: KeySet.t; formals: int list}

let empty = {keys= KeySet.empty; formals= []}

let is_mutex_name name =
  List.mem ["pthread_mutex_t"; "_opaque_pthread_mutex_t"] (Typ.Name.name name) ~equal:String.equal


let is_mutex (typ : Typ.t) = match typ.desc with Tstruct name -> is_mutex_name name | _ -> false

let is_mutex_ptr (typ : Typ.t) = match typ.desc with Tptr (typ, _) -> is_mutex typ | _ -> false

(* the stores of [PTHREAD_RECURSIVE_MUTEX_INITIALIZER[_NP]] that make the mutex [m] recursive:
   [m.__kind = PTHREAD_MUTEX_RECURSIVE_NP] (glibc), [m.__sig = _PTHREAD_RECURSIVE_MUTEX_SIG_init]
   (Darwin) and [m.__private[0] = PTHREAD_MUTEX_RECURSIVE << 14] (bionic) *)
let mutex_of_recursive_initializer_store (lhs : Exp.t) (rhs : Exp.t) =
  let rec eval (exp : Exp.t) =
    match exp with
    | Const (Cint i) ->
        IntLit.to_int i
    | BinOp (BAnd, exp1, exp2) ->
        Option.map2 (eval exp1) (eval exp2) ~f:( land )
    | BinOp (Shiftlt, exp1, exp2) ->
        Option.map2 (eval exp1) (eval exp2) ~f:( lsl )
    | _ ->
        None
  in
  let is_int n exp = Option.exists (eval exp) ~f:(Int.equal n) in
  match lhs with
  | Lfield ({exp}, field, _) -> (
    match Fieldname.get_field_name field with
    | "__kind" when is_int 1 rhs ->
        Some exp
    | "__sig" when is_int 0x32AAABA2 rhs ->
        Some exp
    | _ ->
        None )
  | Lindex (Lfield ({exp}, field, _), index)
    when String.equal (Fieldname.get_field_name field) "__private"
         && is_int 0 index
         && is_int (1 lsl 14) rhs ->
      Some exp
  | _ ->
      None


let is_recursive_type_arg ~attr_typ (kind : Exp.t) =
  match kind with
  | Const (Cint i) ->
      let darwin =
        match attr_typ.Typ.desc with
        | Tptr ({desc= Tstruct name}, _) ->
            String.equal (Typ.Name.name name) "_opaque_pthread_mutexattr_t"
        | _ ->
            false
      in
      IntLit.eq i (IntLit.of_int (if darwin then 2 else 1))
  | _ ->
      (* the type may be recursive *)
      true


let scan_cache : scan Procname.Cache.t = Procname.Cache.create ~name:"recursive_mutex_init"

let rec scan_proc_name ~visiting pname =
  if Procname.Set.mem pname visiting then empty
  else
    match Procname.Cache.lookup scan_cache pname with
    | Some scan ->
        scan
    | None ->
        let scan =
          Procdesc.load pname
          |> Option.value_map ~default:empty
               ~f:(scan_proc ~visiting:(Procname.Set.add pname visiting))
        in
        Procname.Cache.add scan_cache pname scan ;
        scan


and scan_proc ~visiting pdesc =
  let formals = Procdesc.get_pvar_formals pdesc in
  let is_formal pvar = List.exists formals ~f:(fun (formal, _) -> Pvar.equal formal pvar) in
  let loads, local_stores =
    Procdesc.fold_instrs pdesc ~init:(Ident.Map.empty, Pvar.Map.empty)
      ~f:(fun ((loads, local_stores) as acc) _ (instr : Sil.instr) ->
        match instr with
        | Load {id; e} ->
            (Ident.Map.add id e loads, local_stores)
        | Store {e1= Lvar pvar; e2} when not (Pvar.is_global pvar || is_formal pvar) ->
            ( loads
            , Pvar.Map.update pvar (fun stored -> Some (Option.is_none stored, e2)) local_stores )
        | _ ->
            acc )
  in
  (* the value of a local stored only once *)
  let local_value pvar =
    Pvar.Map.find_opt pvar local_stores
    |> Option.bind ~f:(fun (single, value) -> Option.some_if single value)
  in
  (* identifies the value loaded from an address with the address, or with the value of a local
     stored only once *)
  let rec normalize ?(seen = Pvar.Set.empty) (exp : Exp.t) : Exp.t =
    match exp with
    | Var id -> (
      match Ident.Map.find_opt id loads with
      | Some (Lvar pvar as address) when not (Pvar.Set.mem pvar seen) ->
          local_value pvar
          |> Option.value_map ~default:address ~f:(normalize ~seen:(Pvar.Set.add pvar seen))
      | Some address ->
          normalize ~seen address
      | None ->
          exp )
    | Cast (_, exp) ->
        normalize ~seen exp
    | Lfield (obj, field, typ) ->
        Lfield ({obj with exp= normalize ~seen obj.exp}, field, typ)
    | Lindex (array, index) ->
        Lindex (normalize ~seen array, index)
    | _ ->
        exp
  in
  let mutexes, recursive_attrs, inits, copies =
    Procdesc.fold_instrs pdesc ~init:(Exp.Set.empty, Exp.Set.empty, [], [])
      ~f:(fun ((mutexes, recursive_attrs, inits, copies) as acc) _ (instr : Sil.instr) ->
        match instr with
        | Store {e1; typ; e2} -> (
            let e1 = normalize e1 in
            match mutex_of_recursive_initializer_store e1 e2 with
            | Some mutex ->
                (Exp.Set.add mutex mutexes, recursive_attrs, inits, copies)
            | None when is_mutex typ ->
                (mutexes, recursive_attrs, inits, (e1, normalize e2) :: copies)
            | None ->
                acc )
        | Call (_, Const (Cfun callee), args, _, _) -> (
            let is_mutex_method =
              Procname.get_class_type_name callee |> Option.exists ~f:is_mutex_name
            in
            match (Procname.get_method callee, args) with
            | "pthread_mutexattr_settype", [(attr, attr_typ); (kind, _)]
              when is_recursive_type_arg ~attr_typ kind ->
                (mutexes, Exp.Set.add (normalize attr) recursive_attrs, inits, copies)
            | "pthread_mutex_init", [(mutex, _); (attr, _)] ->
                (mutexes, recursive_attrs, (normalize mutex, normalize attr) :: inits, copies)
            | _, [(dst, _); (src, _)] when is_mutex_method ->
                (* copy constructor or assignment *)
                (mutexes, recursive_attrs, inits, (normalize dst, normalize src) :: copies)
            | _ when List.exists args ~f:(fun (_, typ) -> is_mutex_ptr typ) ->
                let {formals} = scan_proc_name ~visiting callee in
                let mutexes =
                  List.foldi args ~init:mutexes ~f:(fun i mutexes (arg, _) ->
                      if List.mem formals i ~equal:Int.equal then
                        Exp.Set.add (normalize arg) mutexes
                      else mutexes )
                in
                (mutexes, recursive_attrs, inits, copies)
            | _ ->
                acc )
        | _ ->
            acc )
  in
  let mutexes =
    List.fold inits ~init:mutexes ~f:(fun mutexes (mutex, attr) ->
        if Exp.Set.mem attr recursive_attrs then Exp.Set.add mutex mutexes else mutexes )
  in
  let rec add_copies mutexes =
    let mutexes' =
      List.fold copies ~init:mutexes ~f:(fun mutexes (dst, src) ->
          if Exp.Set.mem src mutexes then Exp.Set.add dst mutexes else mutexes )
    in
    if Exp.Set.equal mutexes mutexes' then mutexes else add_copies mutexes'
  in
  Exp.Set.fold
    (fun mutex ({keys; formals= recursive_formals} as scan) ->
      match mutex with
      | Lvar pvar when Pvar.is_global pvar ->
          {scan with keys= KeySet.add (Global (AbstractAddress.global_key pvar)) keys}
      | Lvar pvar -> (
        match List.findi formals ~f:(fun _ (formal, _) -> Pvar.equal formal pvar) with
        | Some (i, _) ->
            {scan with formals= i :: recursive_formals}
        | None ->
            scan )
      | Lfield (_, field, _) ->
          {scan with keys= KeySet.add (Field field) keys}
      | _ ->
          scan )
    (add_copies mutexes) empty


let file_cache : KeySet.t SourceFile.Cache.t =
  SourceFile.Cache.create ~name:"recursive_mutex_init_files"


let keys_of_file file =
  match SourceFile.Cache.lookup file_cache file with
  | Some keys ->
      keys
  | None ->
      let keys =
        SourceFiles.proc_names_of_source file
        |> List.fold ~init:KeySet.empty ~f:(fun keys pname ->
            Procdesc.load pname
            |> Option.value_map ~default:keys ~f:(fun pdesc ->
                KeySet.union keys (scan_proc ~visiting:Procname.Set.empty pdesc).keys ) )
      in
      SourceFile.Cache.add file_cache file keys ;
      keys


let constructors tenv class_name =
  Tenv.lookup tenv class_name
  |> Option.value_map ~default:[] ~f:(fun {Struct.methods} ->
      List.filter_map methods ~f:(fun meth ->
          let pname = Struct.name_of_tenv_method meth in
          Option.some_if (Procname.is_constructor pname) pname ) )


let is_recursive tenv ~caller lock_type lock =
  match AbstractAddress.get_last_field_or_global lock with
  | Some field_or_global when is_mutex_name lock_type ->
      let key, initializers =
        match field_or_global with
        | First field ->
            (Field field, constructors tenv (Fieldname.get_class_name field))
        | Second global ->
            ( Global (AbstractAddress.global_key global)
            , Option.to_list (Pvar.get_initializer_pname global) )
      in
      List.exists initializers ~f:(fun pname ->
          KeySet.mem key (scan_proc_name ~visiting:Procname.Set.empty pname).keys )
      || Attributes.load caller
         |> Option.exists ~f:(fun {ProcAttributes.translation_unit} ->
             KeySet.mem key (keys_of_file translation_unit) )
  | _ ->
      false
