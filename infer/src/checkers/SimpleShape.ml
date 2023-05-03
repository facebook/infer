(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module CFG = ProcCfg.NormalOneInstrPerNode

module Ilist = struct
  (** Size-indexed lists *)

  (** These will be used for traversing "in parallel" the shape information structures, accumulating
      a set of a generic (but static) number of field sequences.

      For instance, searching the possible fields of a single variable will return a set of
      one-sized lists of field sequences. Searching the corresponding fields of two
      (variable+fields) of the same shape will return a set of two-sized lists of field sequences.

      Having the size as part of the type allows both having one generic code (parameterised over
      that size type) being able to perform these two traversals, and statically know at the call
      sites how many field sequences we are accumulating. *)

  (** Natural integers as types *)

  type z = |

  type 'a s = |

  (** Lists indexed by their length (second parameter) *)
  type (_, _) t = [] : (_, z) t | ( :: ) : ('a * ('a, 'len) t) -> ('a, 'len s) t

  let rec for_all : type len. ('a, len) t -> f:('a -> bool) -> bool =
   fun li ~f -> match li with [] -> true | x :: xs -> f x && for_all ~f xs


  let rec map : type len. ('a, len) t -> f:('a -> 'b) -> ('b, len) t =
   fun li ~f ->
    match li with
    | [] ->
        []
    | x :: xs ->
        let r = f x in
        r :: map ~f xs


  let rec map2 : type len. ('a, len) t -> ('b, len) t -> f:('a -> 'b -> 'c) -> ('c, len) t =
   fun li li' ~f ->
    match (li, li') with
    | [], [] ->
        []
    | x :: xs, y :: ys ->
        let r = f x y in
        r :: map2 ~f xs ys
end

module Id () : sig
  include Unique_id.Id

  val pp : Format.formatter -> t -> unit
end = struct
  (** Generative unique identifiers with some utility functions *)

  include Unique_id.Int63 ()

  let pp fmt x = Format.fprintf fmt "<%a>" Int63.pp (x : t :> Int63.t)
end

module Fields = struct
  (** A module to help manipulating lists of (nested) fields. *)

  (** We define a type alias to be able use our own json exporting function *)
  type fieldname = Fieldname.t [@@deriving compare, equal, sexp]

  let yojson_of_fieldname fieldname =
    (* The ppx-generated [yojson_of] is unnecessarily complex for our purposes (it's a record that
       involves "class_name", which incidentally seems to always be "_".) *)
    `String (Fieldname.to_string fieldname)


  (* The list is to be understood in "syntactic order": [["a"; "b"]] represents the field part of
     [X#a#b].*)
  type t = fieldname list [@@deriving compare, equal, sexp, yojson_of]

  let pp = Fmt.(list ~sep:nop (any "#" ++ Fieldname.pp))
end

(** This analysis aims at inferring all the fields that may be accessed from some variable, possibly
    through aliases.

    The original goal is to be able to use that information in the lineage analysis, by making
    information flow through fields even if they appear in later instructions. The typical use-case
    is when an argument [$argN] of a function is copied into the name [X], and then the function
    returns some field [foo] of [X]: we want to know that we should create the Lineage node
    [$argN#foo] at the same time as the node [$argN]. *)

module Shape : sig
  module Env : sig
    (** The [Env] module manages shape definitions and typing environments within the context of the
        analysis of a function.

        This is mostly done through a stateful API to ease the implementation that currently heavily
        relies on a union-find data structure (which is easier to implement imperatively). *)

    (** A shape is a handle to which may be attached some subscriptable fields. These fields will
        themselves recursively have an associated shape. Variables will also have a shape associated
        to them. *)
    type shape

    (** An environment associates shapes to variables and fields to shapes. *)
    type t

    val pp : Format.formatter -> t -> unit

    val create_shape : unit -> shape
    (** Create a fresh shape, not associated to any variable yet, and having no known field. *)

    val create : unit -> t
    (** Create a fresh environment with no known variable nor shape fields. *)

    val var_shape : t -> Var.t -> shape
    (** Returns the shape of a variable. If the variable is unknown in the environment, a fresh
        shape will be created, associated to the variable, and returned.

        Will always return a fresh shape for the `_` anonymous erlang variable. *)

    val field_shape : t -> shape -> Fieldname.t -> shape
    (** Given a shape, a subscripted field and an environment, returns the shape of the field
        content. If this field is unknown for that shape in the environment, a fresh shape will be
        created, associated to the field and returned. *)

    val unify : t -> shape -> shape -> unit
    (** Makes two shapes identical by merging their defined fields (unifying the shapes of common
        fields if needed) and making it so that they will both share the same identical set of
        fields in the future (one can now indifferently refer to any of the shapes as an alias of
        the result). *)
  end

  module Summary : sig
    (** Summaries contain the result of the typing analysis of a function, including the types of
        its local variables. They are to be used both for making this shape analysis interprocedural
        (by relating the return types of callee functions to those of their parameters) and for
        using its results later on subsequent analysis (in particular Lineage). *)
    type t

    val pp : Format.formatter -> t -> unit

    val make : Env.t -> t
    (** Makes a summary from a typing environment. Further updates to the environment will have no
        effect on the summary. *)

    val fold_terminal_fields :
         t
      -> Var.t * Fields.t
      -> max_width:int
      -> max_depth:int
      -> prevent_cycles:bool
      -> init:'accum
      -> f:('accum -> Fields.t -> 'accum)
      -> 'accum
    (* Doc in .mli *)

    val fold_terminal_fields_2 :
         t
      -> Var.t * Fields.t
      -> Var.t * Fields.t
      -> max_width:int
      -> max_depth:int
      -> prevent_cycles:bool
      -> init:'accum
      -> f:('accum -> Fields.t -> Fields.t -> 'accum)
      -> 'accum
    (* Doc in .mli *)

    val introduce : formals:Var.t list -> return:Var.t -> t -> Env.t -> Env.shape list * Env.shape
    (** Generates fresh shapes into a typing environment for the formal parameters and the formal
        return of a function. The summary of the function will be used to also introduce shapes for
        the fields of these formal variables, and to link the parameters and/or the return value
        together. The shapes will be returned, but not linked to any variable already present in the
        environment and are to be unified with the actual parameters and return destination. *)
  end
end = struct
  (* Hashtable from/to iterators *)

  let iter_hashtbl htbl f = Hashtbl.iteri ~f:(fun ~key ~data -> f (key, data)) htbl

  let caml_hashtbl_of_iter iter =
    let r = Caml.Hashtbl.create 42 in
    iter (fun (key, data) -> Caml.Hashtbl.add r key data) ;
    r


  (* Pretty-printing *)

  let pp_arrow = Fmt.any " ->@ "

  let pp_binding ~bind pp_key pp_value fmt (key, value) =
    Format.fprintf fmt "@[%a%a%a@]" pp_key key bind () pp_value value


  let pp_hashtbl ~bind pp_key pp_value fmt hashtbl =
    let sep = Fmt.semi in
    let pp_binding = pp_binding ~bind pp_key pp_value in
    Format.fprintf fmt "@[(%a)@]"
      (IFmt.Labelled.iter_bindings ~sep Hashtbl.iteri pp_binding)
      hashtbl


  let pp_caml_hashtbl ~bind pp_key pp_value fmt hashtbl =
    let sep = Fmt.semi in
    let pp_binding = pp_binding ~bind pp_key pp_value in
    Format.fprintf fmt "@[(%a)@]" (Fmt.hashtbl ~sep pp_binding) hashtbl


  module Env = struct
    (** A shape id is what links variables to defined fields. It is a unique identifier to which a
        set of fields is associated, and that will be indirectly assigned to every variable.

        Shape ids do not make sense by themselves and are only valid within the context of the
        analysis of the function that defined them, that is, for a given environment. OCaml typing
        should prevent them from being used elsewhere (for instance mixed in another environment). *)
    module Shape_id =
    Id ()

    (** A shape is an equivalence class of shape identifiers, that is, shape identifiers that share
        a common determined set of fields. *)
    type shape = Shape_id.t Union_find.t

    let sexp_of_shape x = [%sexp_of: _] (Union_find.get x)

    type fields = (Fieldname.t, shape) Hashtbl.t [@@deriving sexp_of]

    (** An environment associates to each variable its equivalence class, and to each possible
        representant of an equivalence class the set of known fields. *)
    type t = {var_shapes: (Var.t, shape) Hashtbl.t; shape_fields: (Shape_id.t, fields) Hashtbl.t}

    let create () =
      {var_shapes= Hashtbl.create (module Var); shape_fields= Hashtbl.create (module Shape_id)}


    let pp_shape fmt x = Shape_id.pp fmt (Union_find.get x)

    let pp fmt {var_shapes; shape_fields} =
      Format.fprintf fmt "@[<v>@[<v4>VAR_SHAPES@ @[%a@]@]@ @[<v4>SHAPE_FIELDS@ @[%a@]@]@]"
        (pp_hashtbl ~bind:pp_arrow Var.pp pp_shape)
        var_shapes
        (pp_hashtbl ~bind:pp_arrow Shape_id.pp
           (pp_hashtbl ~bind:IFmt.colon_sp Fieldname.pp pp_shape) )
        shape_fields


    let create_shape () =
      let id = Shape_id.create () in
      Union_find.create id


    let var_shape {var_shapes; _} var = Hashtbl.find_or_add ~default:create_shape var_shapes var

    let field_shape {shape_fields; _} shape fieldname =
      (* Proceed in two steps: retrieve the field set of this shape or create it, then return the shape
         of the asked field or create it. *)
      let id = Union_find.get shape in
      let field_table =
        Hashtbl.find_or_add ~default:(fun () -> Hashtbl.create (module Fieldname)) shape_fields id
      in
      Hashtbl.find_or_add ~default:create_shape field_table fieldname


    let unify_step shape_fields shape shape' todo =
      (* Unify the shapes and put in the todo stack all the shapes of their fields that should also be unified. *)
      if Union_find.same_class shape shape' then
        (* We need to explicitly check that we are not trying to unify already unified classes to ensure
           termination. Otherwise, given a recursive shape such as [<0> -> { tail : <0> }], unifying
           [ <0> ] with itself would recursively try to unify its fields, therefore recursively
           proceeding to unify [ <0> ] with itself and so on.

           This check guarantees termination because shape unification can now only strictly reduce the
           number of equivalence classes, which is finite. *)
        ()
      else
        let id = Union_find.get shape in
        let id' = Union_find.get shape' in
        Union_find.union shape shape' ;
        match (Hashtbl.find shape_fields id, Hashtbl.find shape_fields id') with
        | None, None ->
            (* No subfield to unify *)
            ()
        | Some _, None ->
            (* Only one shape has fields, just use it as the new representative *)
            Union_find.set shape id
        | None, Some _ ->
            Union_find.set shape id'
        | Some fields, Some fields' ->
            (* Both shapes have fields. We arbitrarily use the second id as a representative then
               merge the second fields into the first ones. During this merge, if we encounter
               a field present in both shapes, we put the shapes of this field in the todo stack to
               unify them at a later step (when the field table of the current processed shapes will
               be completed). *)
            Union_find.set shape id ;
            Hashtbl.merge_into ~src:fields' ~dst:fields ~f:(fun ~key:_fieldname shape' shape_opt ->
                Option.iter ~f:(fun shape -> Stack.push todo (shape, shape')) shape_opt ;
                Set_to shape' ) ;
            Hashtbl.remove shape_fields id'


    (* Repeat the unification steps until the stack is empty. *)
    let rec unify_stack shape_fields todo =
      match Stack.pop todo with
      | None ->
          ()
      | Some (shape, shape') ->
          unify_step shape_fields shape shape' todo ;
          unify_stack shape_fields todo


    let unify {shape_fields; _} shape shape' =
      unify_stack shape_fields (Stack.singleton (shape, shape'))
  end

  module Summary = struct
    (* A summary is similar to the (final) typing environment of a function, with two differences:
        - It uses Caml's hashtables, that can safely be marshalled
        - It "freezes" the union-find shapes into some fixed marshallable values (which are still
          essentially shapes ids, but with a different types to prevent inadvertent mixings) *)

    (* Shape_id for use within the summary -- incompatible with ids from the environment *)
    module Shape_id = Id ()

    (* Caml hashtables are marshallables, Core ones contain functional values *)
    type fields = (Fieldname.t, Shape_id.t) Caml.Hashtbl.t

    (** This is essentially a "frozen" and marshallable version of environments *)
    type t =
      { var_shapes: (Var.t, Shape_id.t) Caml.Hashtbl.t
      ; shape_fields: (Shape_id.t, fields) Caml.Hashtbl.t }

    let pp fmt {var_shapes; shape_fields} =
      Format.fprintf fmt
        "@[<v>@[<v4>SUMMARY VAR SHAPES@ @[%a@]@]@ @[<v4>SUMMARY SHAPES FIELDS@ @[%a@]@]@]"
        (pp_caml_hashtbl ~bind:pp_arrow Var.pp Shape_id.pp)
        var_shapes
        (pp_caml_hashtbl ~bind:pp_arrow Shape_id.pp
           (pp_caml_hashtbl ~bind:IFmt.colon_sp Fieldname.pp Shape_id.pp) )
        shape_fields


    let find_var_shape var_shapes var =
      match Caml.Hashtbl.find_opt var_shapes var with
      | Some shape ->
          shape
      | None ->
          L.die InternalError "No shape found for var %a" Var.pp var


    let find_field_table shape_fields shape =
      match Caml.Hashtbl.find_opt shape_fields shape with
      | Some field_table ->
          field_table
      | None ->
          L.die InternalError "No field table found for shape %a" Shape_id.pp shape


    let find_next_field_shape shape_fields shape field =
      let field_table = find_field_table shape_fields shape in
      match Caml.Hashtbl.find_opt field_table field with
      | Some field_shape ->
          field_shape
      | None ->
          L.die InternalError "Field %a unknown for shape %a.@ Known fields are:@ @[{%a}@]"
            Fieldname.pp field Shape_id.pp shape
            (Fmt.iter_bindings ~sep:Fmt.semi Caml.Hashtbl.iter @@ Fmt.pair Fieldname.pp Shape_id.pp)
            field_table


    let find_field_shape shape_fields shape fields =
      List.fold
        ~f:(fun shape field -> find_next_field_shape shape_fields shape field)
        ~init:shape fields


    let make {Env.var_shapes; shape_fields} =
      (* Making a summary from an environment essentially amounts to converting Env ids into Summary
         ids and Env (Core) hashtables into Summary (Caml) hasthables. We keep an id translation
         table that maps env ids into summary ids and generate a fresh summary id whenever we
         encounter a new env id. *)
      let id_translation_tbl = Hashtbl.create (module Env.Shape_id) in
      let translate_shape_id env_shape_id =
        Hashtbl.find_or_add id_translation_tbl ~default:Shape_id.create env_shape_id
      in
      let translate_shape env_shape = translate_shape_id (Union_find.get env_shape) in
      let translate_fields fields =
        iter_hashtbl fields
        |> Iter.map2 (fun fieldname shape -> (fieldname, translate_shape shape))
        |> caml_hashtbl_of_iter
      in
      let var_shapes =
        iter_hashtbl var_shapes
        |> Iter.map2 (fun var shape -> (var, translate_shape shape))
        |> caml_hashtbl_of_iter
      in
      let shape_fields =
        iter_hashtbl shape_fields
        |> Iter.map2 (fun shape fields -> (translate_shape_id shape, translate_fields fields))
        |> caml_hashtbl_of_iter
      in
      {var_shapes; shape_fields}


    (* Introducing a (callee) summary is not as simple as freezing an environment into a summary,
       because the summary also contains shape fields of all the local variables of the callee that
       we do not want to put into the caller environment. Therefore we proceed by only introducing
       the shapes of some explicit variables (that will be the formals and return of the callee), and
       recursively discovering and introducing their fields. *)
    let rec introduce_shape id_translation_tbl shape_id shape_fields env_shape_fields =
      (* Translate and introduce a shape from the summary into the environment shapes *)
      match Hashtbl.find id_translation_tbl shape_id with
      | Some env_shape ->
          (* If the shape is already present in the table, then it has already been introduced
             earlier. Just return its translation. *)
          env_shape
      | None ->
          (* This is a new shape to translate. Create a fresh environment shape and populate it by
             recursively introducing its fields. *)
          let env_shape = Env.create_shape () in
          Hashtbl.set id_translation_tbl ~key:shape_id ~data:env_shape ;
          ( match Caml.Hashtbl.find_opt shape_fields shape_id with
          | None ->
              ()
          | Some fields ->
              Hashtbl.set env_shape_fields ~key:(Union_find.get env_shape)
                ~data:(introduce_fields id_translation_tbl fields shape_fields env_shape_fields) ) ;
          env_shape


    and introduce_fields id_translation_tbl fields shape_fields env_shape_fields =
      let env_fields = Hashtbl.create (module Fieldname) in
      Caml.Hashtbl.iter
        (fun fieldname shape_id ->
          Hashtbl.set env_fields ~key:fieldname
            ~data:(introduce_shape id_translation_tbl shape_id shape_fields env_shape_fields) )
        fields ;
      env_fields


    let introduce_var ~var id_translation_tbl {var_shapes; shape_fields}
        {Env.shape_fields= env_shape_fields; _} =
      introduce_shape id_translation_tbl
        (Caml.Hashtbl.find var_shapes var)
        shape_fields env_shape_fields


    let introduce ~formals ~return summary env =
      (* [id_translation_tbl] maps Ids from the summary to their translation as Ids in the
         environment of the caller function. *)
      let id_translation_tbl = Hashtbl.create (module Shape_id) in
      (* We introduce into the (caller) environment the *formal* parameters and return value from the
         (callee) summary. It will be the responsibility of the call-interpretation code to then
         unify these formals with the actual parameters and ret_id that already live in the caller
         environment. *)
      let args_env_shapes =
        List.map ~f:(fun arg -> introduce_var ~var:arg id_translation_tbl summary env) formals
      in
      let return_env_shape = introduce_var ~var:return id_translation_tbl summary env in
      (args_env_shapes, return_env_shape)


    let pp_field_table field_table =
      Fmt.iter_bindings ~sep:Fmt.comma Caml.Hashtbl.iter
        (Fmt.pair ~sep:IFmt.colon_sp Fieldname.pp Shape_id.pp)
        field_table


    (** Boxing fields are fields that are internally generated by the frontend to box scalar values,
        such as integer or atoms. They should not be considered as actual fields by the Lineage
        analysis. *)
    let is_boxing_field fieldname =
      Array.mem ~equal:Fieldname.equal
        [| Fieldname.make (ErlangType Atom) ErlangTypeName.atom_value
         ; Fieldname.make (ErlangType Atom) ErlangTypeName.atom_hash
         ; Fieldname.make (ErlangType Integer) ErlangTypeName.integer_value |]
        fieldname


    let finalise_prefixes shape_fields max_width max_depths prevent_cycles root_shapes rev_prefixes
        =
      let rev_and_trim_depth max_depth rev_prefix =
        (* Reverse the reversed prefixes and trim them to the max depths *)
        List.rev (List.drop rev_prefix (-max_depth))
      in
      let rec enforce_width_and_cycles cycle_set shape_fields max_width shape = function
        (* Walk through the fields and ensure that we do not:
           - Traverse a field table wider than max_width
           - Traverse cycles while forbidden by the option
           - "Traverse" (they must be in last position) internal "boxing" fields that should be
             ignored
        *)
        | [] ->
            []
        | [field] when is_boxing_field field ->
            []
        | field :: _ when is_boxing_field field ->
            L.die InternalError "SimpleShape: unexpected boxing field in non tail position."
        | field :: fields ->
            let field_table = find_field_table shape_fields shape in
            if
              Caml.Hashtbl.length field_table > max_width
              || (prevent_cycles && Set.mem cycle_set shape)
            then []
            else
              field
              :: enforce_width_and_cycles (Set.add cycle_set shape) shape_fields max_width
                   (Caml.Hashtbl.find field_table field)
                   fields
      in
      rev_prefixes
      |> Ilist.map2 ~f:rev_and_trim_depth max_depths
      |> Ilist.map2
           ~f:(enforce_width_and_cycles (Set.empty (module Shape_id)) shape_fields max_width)
           root_shapes


    (** Given field shapes, a particular shape, a maximal width and three size-indexed lists of
        maximal depths, var shapes (cf. later) and reversed field prefixes, traverses the field
        shapes table and builds in parallel the sequence of field prefixes by recursively adding all
        the defined fieldname of the considered shape to the prefixes.

        The [var_shapes] list is expected to contain the list of the shapes of the variables from
        which the fields [rev_prefixes] are being taken. It will be used to ensure that the result
        will not cross a field table wider than [max_width] even if one of the argument does. For
        instance, the terminal fields of [X#field1], where [X] itself has a huge number of fields,
        will only be [X].

        A shape having more than max_width fields will be considered as "terminal", ie. not having
        any field at all.

        Once every possible subfield has been traversed or the maximal depth has been reached for
        every prefix, the traversal will consider that a "terminal" field has been reached (and the
        folding function will be called). *)
    let rec fold_terminal_fields_of_shape shape_fields shape ~max_width ~max_depths ~prevent_cycles
        ~traversed ~root_shapes ~rev_prefixes ~init ~f =
      if
        Ilist.for_all ~f:Int.is_non_positive max_depths
        || (prevent_cycles && Set.mem traversed shape)
      then
        f init
          (finalise_prefixes shape_fields max_width max_depths prevent_cycles root_shapes
             rev_prefixes )
      else
        match Caml.Hashtbl.find_opt shape_fields shape with
        | None ->
            f init
              (finalise_prefixes shape_fields max_width max_depths prevent_cycles root_shapes
                 rev_prefixes )
        | Some fields ->
            let len = Caml.Hashtbl.length fields in
            if Int.(len = 0 || len > max_width) then
              f init
                (finalise_prefixes shape_fields max_width max_depths prevent_cycles root_shapes
                   rev_prefixes )
            else
              let traversed = Set.add traversed shape in
              Caml.Hashtbl.fold
                (fun fieldname fieldshape acc ->
                  let rev_prefixes = Ilist.map ~f:(List.cons fieldname) rev_prefixes in
                  let max_depths = Ilist.map ~f:Int.pred max_depths in
                  fold_terminal_fields_of_shape shape_fields fieldshape ~max_width ~max_depths
                    ~root_shapes ~traversed ~prevent_cycles ~rev_prefixes ~init:acc ~f )
                fields init


    let fold_terminal_fields {var_shapes; shape_fields} (var, fields) ~max_width ~max_depth
        ~prevent_cycles ~init ~f =
      let var_shape = find_var_shape var_shapes var in
      let shape = find_field_shape shape_fields var_shape fields in
      let root_shapes = Ilist.[var_shape] in
      let rev_prefixes = Ilist.[List.rev fields] in
      let max_depths = Ilist.[max_depth - List.length fields] in
      let traversed = Set.empty (module Shape_id) in
      fold_terminal_fields_of_shape shape_fields shape ~max_width ~max_depths ~rev_prefixes
        ~root_shapes ~traversed ~prevent_cycles
        ~f:(fun acc Ilist.[x] -> f acc x)
        ~init


    let fold_terminal_fields_2 {var_shapes; shape_fields} (var1, fields1) (var2, fields2) ~max_width
        ~max_depth ~prevent_cycles ~init ~f =
      let var_shape1 = find_var_shape var_shapes var1 in
      let var_shape2 = find_var_shape var_shapes var2 in
      let shape1 = find_field_shape shape_fields var_shape1 fields1 in
      let shape2 = find_field_shape shape_fields var_shape2 fields2 in
      if not ([%equal: Shape_id.t] shape1 shape2) then
        L.die InternalError
          "@[Attempting to get related fields of differently shaped fields: @[%a={%a}@]@ vs@ \
           @[%a={%a}@]@]"
          Shape_id.pp shape1 pp_field_table
          (Caml.Hashtbl.find shape_fields shape1)
          Shape_id.pp shape2 pp_field_table
          (Caml.Hashtbl.find shape_fields shape1)
      else
        let max_depths = Ilist.[max_depth - List.length fields1; max_depth - List.length fields2] in
        let rev_prefixes = Ilist.[List.rev fields1; List.rev fields2] in
        let root_shapes = Ilist.[var_shape1; var_shape2] in
        let traversed = Set.empty (module Shape_id) in
        fold_terminal_fields_of_shape shape_fields shape1 ~max_width ~max_depths ~rev_prefixes
          ~root_shapes ~traversed ~prevent_cycles
          ~f:(fun acc Ilist.[x; y] -> f acc x y)
          ~init
  end
end

(** As the environment works imperatively, we do not need to propagate it and therefore use a simple
    Top/Bottom abstract domain that will simply remember which states have been reached yet. Note
    that the resulting analysis is flow insensitive, as the same global shape environment will be
    augmented during the traversal of a procedure code, and no intermediary version of it will be
    associated to the individual control flow vertices. *)
module Domain = AbstractDomain.Unit

module Summary = Shape.Summary

module Report = struct
  (** Reporting utility module. *)

  let debug proc_desc env summary =
    (* Print a local environment, a summary and the fields of the returned value in the debug
       logs. *)
    let procname = Procdesc.get_proc_name proc_desc in
    L.debug Analysis Verbose "@[<v>@ @[<v2>" ;
    L.debug Analysis Verbose "@[<v>Result for procedure : %a@]@ " Procname.pp procname ;
    L.debug Analysis Verbose "@[<v2>LOCAL ENV:@ %a@]@ @ " Shape.Env.pp env ;
    L.debug Analysis Verbose "@[<v2>SUMMARY:@ %a@]@ @ " Shape.Summary.pp summary ;
    L.debug Analysis Verbose "@[<v2>FIELDS OF RETURN:@ (%a)@]"
      (Fmt.iter
         (fun f summary ->
           Shape.Summary.fold_terminal_fields summary ~max_depth:3 ~max_width:5 ~prevent_cycles:true
             (Var.of_pvar (Procdesc.get_ret_var proc_desc), [])
             ~f:(fun () fields -> f fields)
             ~init:() )
         ~sep:Fmt.comma
         (Fmt.list ~sep:(Fmt.any "#") Fieldname.pp) )
      summary ;
    L.debug Analysis Verbose "@]@ @]"
end

(** Transfer functions to compute shapes. As the environment is an imperative structure, this module
    takes a global environment as a parameter that will be mutated through the analysis by the
    hereby defined transfer functions. *)
module TransferFunctions (Env : sig
  val env : Shape.Env.t
end) =
struct
  module Domain = Domain
  module CFG = CFG

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let env = Env.env

  (** Returns the shape of an expression. Fresh shapes will be created as needed. *)
  let rec shape_expr (e : Exp.t) =
    match e with
    | Const _ | Closure _ ->
        (* We use fresh ids to represent shapes that do not hold fields. *)
        Shape.Env.create_shape ()
    | Var id ->
        Shape.Env.var_shape env (Var.of_id id)
    | Lvar pvar ->
        Shape.Env.var_shape env (Var.of_pvar pvar)
    | Lfield (e, fieldname, _) ->
        let shape_e = shape_expr e in
        Shape.Env.field_shape env shape_e fieldname
    | Sizeof {dynamic_length= None} ->
        Shape.Env.create_shape ()
    | UnOp (_, e, _) | Exn e | Cast (_, e) | Sizeof {dynamic_length= Some e} ->
        (* We first shape [e] to possibly discover some fields (eg. on [not (x.f)]), then return
           a fresh id as unary operators only return scalar value. *)
        ignore (shape_expr e : Shape.Env.shape) ;
        Shape.Env.create_shape ()
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        (* Similar to the UnOp case *)
        ignore (shape_expr e1 : Shape.Env.shape) ;
        ignore (shape_expr e2 : Shape.Env.shape) ;
        Shape.Env.create_shape ()


  module CallModel = struct
    let make_tuple ret_id args =
      (* Unify the shapes of the fields of the return with the shapes of the arguments *)
      let ret_shape = Shape.Env.var_shape env ret_id in
      let tuple_type : Typ.name = ErlangType (Tuple (List.length args)) in
      let fieldname i = Fieldname.make tuple_type (ErlangTypeName.tuple_elem (i + 1)) in
      let ret_field_shape i = Shape.Env.field_shape env ret_shape (fieldname i) in
      List.iteri ~f:(fun i arg -> Shape.Env.unify env (shape_expr arg) (ret_field_shape i)) args


    let get_custom_model procname =
      let models = [(BuiltinDecl.__erlang_make_tuple, make_tuple)] in
      List.Assoc.find ~equal:Procname.equal models procname


    let ignore_shape_ret_and_args ret_var args =
      ignore (Shape.Env.var_shape env ret_var : Shape.Env.shape) ;
      ignore (List.map ~f:shape_expr args : Shape.Env.shape list) ;
      ()


    let unknown_model procname ret_var args =
      L.debug Analysis Verbose "@[<v2> SimpleShape: no model found for expression `%a`@]@,"
        Procname.pp procname ;
      ignore_shape_ret_and_args ret_var args ;
      ()


    let standard_model procname summary ret_var args =
      (* Standard call of a known function:
         1. We get the shape of the actual args and ret_id
         2. We introduce into the environment the shapes of the formal args and return value of
            the function, obtained from the summary.
         3. We unify the actual and formal params/return together.
         Eventually the ret_id shape will therefore correctly be related to the shapes of the
         actual parameters of the function.
      *)
      let ret_id_shape = Shape.Env.var_shape env ret_var in
      let actual_args_shapes = List.map ~f:shape_expr args in
      let return = Var.of_pvar (Pvar.get_ret_pvar procname) in
      let formals =
        Attributes.load_exn procname |> ProcAttributes.get_pvar_formals
        |> List.map ~f:(fun (pvar, _typ) -> Var.of_pvar pvar)
      in
      let formal_shapes, returned_shape = Shape.Summary.introduce ~return ~formals summary env in
      List.iter2_exn ~f:(fun s1 s2 -> Shape.Env.unify env s1 s2) actual_args_shapes formal_shapes ;
      Shape.Env.unify env ret_id_shape returned_shape


    let exec analyze_dependency procname ret_var args =
      match get_custom_model procname with
      | Some model ->
          model ret_var args
      | None -> (
        match analyze_dependency procname with
        | Some summary ->
            standard_model procname summary ret_var args
        | None ->
            unknown_model procname ret_var args )
  end

  let exec_assignment var rhs_exp =
    (* When assigning a value to a variable, we unify the current shape of that variable to the shape
       of the expression, thus merging together the fields that have been collected so far on both
       sides.

       Note that this might lead to over-approximating the field set of a variable that would be
       reassigned in the program with completely unrelated types. We believe that it does not happen
       with the Erlang translation anyway (and even then would not be a fundamental issue). *)
    let var_shape = Shape.Env.var_shape env var in
    let expr_shape = shape_expr rhs_exp in
    Shape.Env.unify env var_shape expr_shape


  let procname_of_exp (e : Exp.t) : Procname.t option =
    match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


  (** Execute an instruction by mutating the environment *)
  let exec_instr_unit {InterproceduralAnalysis.analyze_dependency} (instr : Sil.instr) =
    match instr with
    | Call ((ret_id, _typ), fun_exp, args, _location, _flags) -> (
        let ret_var = Var.of_id ret_id in
        let args = List.map ~f:fst args (* forget SIL types *) in
        match procname_of_exp fun_exp with
        | None ->
            CallModel.ignore_shape_ret_and_args ret_var args ;
            L.debug Analysis Verbose "@[<v>SimpleShape: call of unsupported expression `%a`.@]@,"
              Exp.pp fun_exp
        | Some procname ->
            CallModel.exec analyze_dependency procname ret_var args )
    | Prune (e, _, _, _) ->
        ignore (shape_expr e : Shape.Env.shape)
    | Metadata _ ->
        ()
    | Load {id; e; _} ->
        exec_assignment (Var.of_id id) e
    | Store {e1= Lvar pvar; e2; _} ->
        (* Same as Load *)
        exec_assignment (Var.of_pvar pvar) e2
    | Store _ ->
        L.die InternalError "SimpleShape: Store instructions are only supported with Lvar lhs"


  (** Mutates the environment and then return an abstract state (which is actually the same as the
      parameter). *)
  let exec_instr astate interproc_data _node _instr_index instr =
    let () = exec_instr_unit interproc_data instr in
    astate


  let pp_session_name _node fmt = Format.pp_print_string fmt "SimpleLineageShape"
end

(** A generative module that creates a fresh environment and passes it to the {!TransferFunctions}
    functor to build an analysis engine. *)
module Analyzer () = struct
  module Env = struct
    let env = Shape.Env.create ()
  end

  include AbstractInterpreter.MakeRPO (TransferFunctions (Env))
end

let unskipped_checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let module Analyzer = Analyzer () in
  (* Shape captured vars *)
  let shape_captured_var {CapturedVar.pvar} =
    ignore (Shape.Env.var_shape Analyzer.Env.env (Var.of_pvar pvar) : Shape.Env.shape)
  in
  List.iter ~f:shape_captured_var (Procdesc.get_captured proc_desc) ;
  (* Analyze the procedure's code  *)
  let _invmap : Analyzer.invariant_map = Analyzer.exec_pdesc analysis_data ~initial:() proc_desc in
  let summary = Shape.Summary.make Analyzer.Env.env in
  Report.debug proc_desc Analyzer.Env.env summary ;
  Some summary


let checker analysis_data =
  (* We skip the functions that would not be analysed by SimpleLineage anyway *)
  SimpleLineageUtils.skip_unwanted (fun data () -> unskipped_checker data) analysis_data ()
