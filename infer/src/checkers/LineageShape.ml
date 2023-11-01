(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module CFG = ProcCfg.NormalOneInstrPerNode

module LineageConfig = struct
  let field_depth = Config.lineage_field_depth

  let field_width = Option.value ~default:Int.max_value Config.lineage_field_width

  let variant_width = Config.lineage_variant_width

  let prevent_cycles = Config.lineage_prevent_cycles
end

module FieldLabel = struct
  module T = struct
    type t =
      | Fieldname of Fieldname.t
      | MapKey of string  (** Statically known map keys, ie. currently constant atoms. *)
    [@@deriving compare, equal, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let pp fmt t =
    (* Invariant: distinct labels should always be distinctly printed (eg. differentiate between
       a field and a map key with the same name). *)
    match t with
    | Fieldname fieldname ->
        Fieldname.pp fmt fieldname
    | MapKey key ->
        Fmt.pf fmt "'%s'" key


  let yojson_of_t t =
    (* The ppx-generated [yojson_of] for Fieldname is unnecessarily complex on for our purposes (it's
       a record that involves "class_name", which incidentally seems to always be "_".)

       Therefore we simply use the pretty printed output to generate prettier and simpler json
       export. *)
    `String (Fmt.to_to_string pp t)


  let fieldname f = Fieldname f

  let make_fieldname typ name = Fieldname (Fieldname.make typ name)

  let map_key k = MapKey k
end

module FieldPath = struct
  (** A module to help manipulating lists of (nested) fields. *)

  module T = struct
    (* The list is to be understood in "syntactic order": [["a"; "b"]] represents the field part of
       [X#a#b].*)
    type t = FieldLabel.t list [@@deriving compare, equal, sexp, yojson_of]
  end

  include T
  include Comparable.Make (T)

  let pp = Fmt.(list ~sep:nop (any "#" ++ FieldLabel.pp))
end

module Cell : sig
  (** See .mli for doc *)

  type t [@@deriving compare, equal, sexp, yojson_of]

  val create : var:Var.t -> field_path:FieldPath.t -> is_abstract:bool -> t

  val pp : t Fmt.t

  val var : t -> Var.t

  val field_path : t -> FieldPath.t

  val is_abstract : t -> bool

  val var_appears_in_source_code : t -> bool
end = struct
  type t = {var: Var.t; field_path: FieldPath.t; is_abstract: bool}
  [@@deriving compare, equal, sexp, fields]

  let create = Fields.create

  let pp fmt {var; field_path; is_abstract} =
    Var.pp fmt var ;
    FieldPath.pp fmt field_path ;
    if is_abstract && Config.debug_mode then
      (* We could also consider printing that by default but that could break some other systems
         expectations. *)
      Fmt.pf fmt "#"


  let yojson_of_t x = `String (Fmt.to_to_string pp x)

  let var_appears_in_source_code x = Var.appears_in_source_code (var x)
end

module Id () : sig
  include Unique_id.Id

  val pp : Format.formatter -> t -> unit
end = struct
  (** Generative unique identifiers with some utility functions *)

  include Unique_id.Int63 ()

  let pp fmt x = Format.fprintf fmt "<%a>" Int63.pp (x : t :> Int63.t)
end

(** This analysis aims at inferring all the fields that may be accessed from some variable, possibly
    through aliases.

    The original goal is to be able to use that information in the lineage analysis, by making
    information flow through fields even if they appear in later instructions. The typical use-case
    is when an argument [$argN] of a function is copied into the name [X], and then the function
    returns some field [foo] of [X]: we want to know that we should create the Lineage node
    [$argN#foo] at the same time as the node [$argN]. *)

module Env : sig
  module State : sig
    (** The [State] module manages shape definitions and typing environments within the context of
        the analysis of a function.

        This is mostly done through a stateful API to ease the implementation that currently heavily
        relies on a union-find data structure (which is easier to implement imperatively). *)

    (** A shape is a handle to which may be attached some subscriptable fields. These fields will
        themselves recursively have an associated shape. Variables will also have a shape associated
        to them. *)
    type shape

    (** An environment associates shapes to variables and structures to shapes. *)
    type t

    module Shape : sig
      (** {1} Shape building functions. These functions will create the appropriate structure,
          associate it to a shape in the environment and return that shape. *)

      val variant_of_list : string list -> t -> shape
      (** The shape of a value equal to a string amongst a set of possible ones. *)

      val vector_zero : t -> shape
      (** The shape of a vector that currently has no known field. Usually fields will be discovered
          later on by unification. This can be used as a generic shape for something that might be a
          vector (preventing for instance later unification with atom-only shapes).

          Every call to this function will return a new, distinct shape.

          This is equivalent to calling {!vector_of_alist} with an empty list. *)

      val vector_fully_abstract : t -> shape

      val vector_of_alist : (FieldLabel.t * shape) list -> t -> shape
      (** A shape with a set of known fields. The list may be empty, meaning that the shape may be a
          vector but no field has been discovered yet. *)

      val scalar : t -> shape
      (** The shape of scalar values for which we don't have more precise information (eg.
          integers). *)
    end

    val create : unit -> t
    (** Create a fresh environment with no known variable nor shape. *)

    val shape_var : t -> Var.t -> shape
    (** Returns the shape of a variable. If the variable is unknown in the environment, a fresh
        *empty vector* shape will be created, associated to the variable, and returned. *)

    val shape_record_field : t -> shape -> Fieldname.t -> shape
    (** If the argument shape has the argument fieldname defined, returns the shape of the
        corresponding field. Otherwise create that field with a vector shape and return it (ie.
        unify the argument shape with a vector containing at least the argument fieldname with a
        vector shape S and return this latter vector shape S.) *)

    val shape_map_value : t -> map_shape:shape -> key_shape:shape -> shape
    (** Returns the shape of a map value field, according to the shape of the key used. The value
        field will be created first if needed, and if the key can access multiple map fields, they
        will all be unified. *)

    val unify_map_value : t -> map_shape:shape -> key_shape:shape -> value_shape:shape -> unit
    (** Unifies the shape of a map value field, according to the shape of the key used. If the key
        can access multiple fields, they will all be unified. *)

    val unify : t -> shape -> shape -> unit
    (** Makes two shapes identical by merging their defined fields (unifying the shapes of common
        fields if needed) and making it so that they will both share the same identical set of
        fields in the future (one can now indifferently refer to any of the shapes as an alias of
        the result). *)

    val unify_var : t -> Var.t -> shape -> unit
  end

  module Summary : sig
    (** Summaries contain the result of the typing analysis of a function, including the types of
        its local variables. They are to be used both for making this shape analysis interprocedural
        (by relating the return types of callee functions to those of their parameters) and for
        using its results later on subsequent analysis (in particular Lineage). *)
    type t

    val pp : t Fmt.t
    (** Puts some effort into printing easier-to-understand summaries. Does not try to be
        performance efficient. *)

    val make : State.t -> t
    (** Makes a summary from a state environment. Further updates to the state will have no effect
        on the summary. *)

    val fold_cells :
      t -> Var.t * FieldPath.t -> init:'accum -> f:('accum -> Cell.t -> 'accum) -> 'accum
    (* Doc in .mli *)

    val fold_cell_pairs :
         t
      -> Var.t * FieldPath.t
      -> Var.t * FieldPath.t
      -> init:'accum
      -> f:('accum -> Cell.t -> Cell.t -> 'accum)
      -> 'accum
    (* Doc in .mli *)

    val introduce :
      formals:Var.t list -> return:Var.t -> t -> State.t -> State.shape list * State.shape
    (** Generates fresh shapes into a state environment for the formal parameters and the formal
        return of a function. The summary of the function will be used to also introduce shapes for
        the fields of these formal variables, and to link the parameters and/or the return value
        together. The shapes will be returned, but not linked to any variable already present in the
        state and are to be unified with the actual parameters and return destination. *)
  end
end = struct
  (* Hashtable from/to iterators *)

  let iter_hashtbl htbl f = Hashtbl.iteri ~f:(fun ~key ~data -> f (key, data)) htbl

  let hashtbl_of_iter_exn key_module iter =
    let r = Hashtbl.create key_module in
    iter (fun (key, data) -> Hashtbl.add_exn r ~key ~data) ;
    r


  (* Pretty-printing *)

  let pp_arrow = Fmt.any " ->@ "

  let pp_binding ~bind pp_key pp_value fmt (key, value) =
    Format.fprintf fmt "@[%a%a%a@]" pp_key key bind () pp_value value


  let pp_hashtbl_sorted ~compare ?(sep = Fmt.semi) ~bind pp_key pp_value fmt hashtbl =
    let pp_binding = pp_binding ~bind pp_key pp_value in
    Format.fprintf fmt "@[(%a)@]"
      (IFmt.Labelled.iter ~sep List.iter pp_binding)
      (Hashtbl.to_alist hashtbl |> List.sort ~compare:(fun (k, _) (k', _) -> compare k k'))


  let pp_map ?(sep = Fmt.semi) ~bind pp_key pp_value fmt map =
    let pp_binding = pp_binding ~bind pp_key pp_value in
    Format.fprintf fmt "@[(%a)@]" (IFmt.Labelled.iter_bindings ~sep Map.iteri pp_binding) map


  module Types = struct
    (** Type definitions, common to procedure analysis environments and summaries *)

    module type Shape_class = sig
      (** Shape classes are equivalence classes of shape identifiers, that is, sets of shape
          identifiers that share a common set of fields. *)

      type 'id t

      val pp : 'id Fmt.t -> 'id t Fmt.t
    end

    module Make (Shape_class : Shape_class) () = struct
      (** A functor that defines the common types and fundamental functions of shape environments.

          Takes as parameter the (module) type of shape classes and is generative because it will
          generate a fresh Id module. *)

      (** A shape id is what links variables to defined fields. It is a unique identifier to which a
          structure (eg. set of fields) is associated, and that will be indirectly assigned to every
          variable.

          Shape ids do not make sense by themselves and are only valid within the full context of
          the corresponding environment for which they have been generated. *)
      module Shape_id =
      Id ()

      (** A shape is an equivalence class of shape identifiers. *)
      type shape = Shape_id.t Shape_class.t

      let pp_shape = Shape_class.pp Shape_id.pp

      module Structure : sig
        (** Private type: the constructor functions may do some checks before building structures,
            but we want to allow pattern matching. *)
        type t = private
          | Bottom
              (** The shape of values that have had not been set anywhere yet. This should happen
                  only upon assigning a new variable or variable path. Encountering bottom shapes
                  elsewhere should raise exceptions. *)
          | Variant of String.Set.t
              (** The shape of values that are statically known to only have as possible values a
                  set of atoms. Invariant: the set should not be empty (or unspecified behaviours
                  may trigger). *)
          | LocalAbstract
              (** A local abstract shape typically comes from a function parameter. Inside the
                  current procedure, it cannot be unified with a variant-shape as we cannot make any
                  assumption on its value.

                  However, upon calling this procedure, that shape will not be introduced in the
                  caller environment, meaning the caller is free to unify it with the actual shapes
                  used on the actual arguments.

                  Example:

                  {[
                    f(X) -> case foo of foo -> X; bar -> baz end.

                    main() -> Y = f(ok), Y.
                  ]}

                  Within [id], [X] is locally abstract. Although it shares a shape (as the formal
                  return) with [baz], the unification will not make it an atom (meaning a map access
                  using [X] as a key would be imprecise, for instance). [f$ret] is still inferred to
                  be the same shape as [X].

                  Within [main], the shape of the argument of [f] is actually known to be the
                  singleton variant [\[ok\]]. That argument is not introduced as an abstract shape
                  by the call instruction, which allows actually unifying it with a variant at
                  call-time. [Y] will still have the same shape as that argument as mandated by the
                  summary of [f], therefore [Y] will correctly be inferred as the variant [ok]. *)
          | Vector of
              { is_fully_abstract: bool
                    (** If [is_fully_abstract] is true, then this shape ultimately comes from an
                        unknown procedure result. It can never be unified with a variant shape, nor
                        can any of its to-be-discovered fields.

                        If that field is false, then fields that are discovered in the future will
                        be considered as only locally abstract (they cannot be unified with variants
                        in the same procedure, but could be in some callers). *)
              ; all_map_value: shape option
                    (** We maintain the following invariant: if fields contains [MapKey x : S] and
                        [all_map_value] is [Some S'], then S = S'. Also [all_map_value] is [None]
                        iff all seen map accesses use known atoms as keys.

                        Explanation follows.

                        If a vector semantically has at last one key which cannot be statically
                        determined as a constant atom, then we assume that accessing key may access
                        all map-keys of that vector.

                        In that case all the map keys of the vector must have the same shape, that
                        is, all the atom-map-keys that we keep track of. We thus set [map_value] to
                        that unified shape, and all further map-value shapes will be unified to it.

                        That doesn't concern fieldname keys: if a vector-shaped variable can be
                        either a map or a record, then even if the map has complex keys, accessing
                        them won't be related to the fieldnames of the record case. *)
              ; fields: shape FieldLabel.Map.t }
              (** The shape of values that can have values other than statically known atoms.

                  Remark 1: integers (and possibly other constants) will currently be represented as
                  vectors with an empty list of fields.

                  Remark 2: if a value can be either an atom or something else, we currently forget
                  the atom part altogether and only remember the "something else". The rationale is
                  that atoms are currently used only for precise map accesses, and we cannot do
                  these if the value may be a non-atom. *)

        val pp : t Fmt.t

        val bottom : t

        val variant : String.Set.t -> t
        (** If the set is too big (see {!LineageConfig}), will build a {!scalar} structure instead. *)

        val variant_of_list : string list -> t

        val local_abstract : t

        val vector : is_fully_abstract:bool -> ?all_map_value:shape -> shape FieldLabel.Map.t -> t

        val vector_fully_abstract : t

        val vector_of_alist : (FieldLabel.t * shape) list -> t

        val complex_map : all_map_value:shape -> t
        (** Shape of a map with no known key and a unique value shape. *)

        val scalar : t
        (** Scalar shape for which we don't keep a known set of possible values. *)
      end = struct
        type t =
          | Bottom
          | Variant of String.Set.t
          | LocalAbstract
          | Vector of
              {is_fully_abstract: bool; all_map_value: shape option; fields: shape FieldLabel.Map.t}

        let pp fmt x =
          match x with
          | Bottom ->
              Fmt.pf fmt "[]"
          | LocalAbstract ->
              Fmt.pf fmt "?"
          | Variant constructors ->
              Fmt.pf fmt "[@[%a@]]"
                (IFmt.Labelled.iter ~sep:(Fmt.any "@ |@ ") Set.iter Fmt.string)
                constructors
          | Vector {is_fully_abstract; fields; all_map_value} ->
              (* Example: ('foo' : <1>, 'bar' : <1>, baz : <2>) {* : <1>} *)
              Fmt.pf fmt "@[%t%a%a@]"
                (if is_fully_abstract then Fmt.fmt "T " else Fmt.fmt "")
                (pp_map ~bind:IFmt.colon_sp FieldLabel.pp pp_shape)
                fields
                Fmt.(option ~none:nop (any "@ {* :@ " ++ pp_shape ++ any "}"))
                all_map_value


        let bottom = Bottom

        let vector ~is_fully_abstract ?all_map_value fields =
          Vector {is_fully_abstract; all_map_value; fields}


        let vector_fully_abstract = vector ~is_fully_abstract:true FieldLabel.Map.empty

        let vector_of_alist field_alist =
          vector ~is_fully_abstract:false (FieldLabel.Map.of_alist_exn field_alist)


        let complex_map ~all_map_value =
          vector ~is_fully_abstract:false ~all_map_value FieldLabel.Map.empty


        let scalar =
          (* Until we add a specific constructor, the best abstraction for a scalar value is an empty
             vector. *)
          vector_of_alist []


        let local_abstract = LocalAbstract

        let variant constructors =
          if Int.O.(Set.length constructors <= LineageConfig.variant_width) then
            Variant constructors
          else scalar


        let variant_of_list constructor_list = variant (String.Set.of_list constructor_list)
      end

      (** An environment associates to each variable its equivalence class, and to each shape
          equivalence class representative its structure. *)
      type t =
        {var_shapes: (Var.t, shape) Hashtbl.t; shape_structures: (Shape_id.t, Structure.t) Hashtbl.t}

      let pp fmt {var_shapes; shape_structures} =
        Format.fprintf fmt "@[<v>@[<v4>VAR_SHAPES@ @[%a@]@]@ @[<v4>SHAPE_STRUCTURES@ @[%a@]@]@]"
          (pp_hashtbl_sorted ~compare:Var.compare ~bind:pp_arrow Var.pp pp_shape)
          var_shapes
          (pp_hashtbl_sorted ~compare:Shape_id.compare ~bind:pp_arrow Shape_id.pp Structure.pp)
          shape_structures
    end
  end

  module State = struct
    (** Environments for the in-progress analysis of a procedure. *)

    module Shape_class = struct
      (** Equivalence classes are stored in a Union-find data structures. That allows cheap merging
          of shapes that are inferred to be equivalent during the procedure analysis. *)

      type 'id t = 'id Union_find.t

      let pp pp_id = Fmt.using Union_find.get pp_id
    end

    include Types.Make (Shape_class) ()

    let create () =
      {var_shapes= Hashtbl.create (module Var); shape_structures= Hashtbl.create (module Shape_id)}


    module Shape = struct
      module Private = struct
        (** Functions that should not be used outside the [Env] module *)

        (** create a shape but without any structure yet. *)
        let create () =
          let id = Shape_id.create () in
          Union_find.create id


        let create_and_store structure {shape_structures; _} =
          let id = Shape_id.create () in
          Hashtbl.set shape_structures ~key:id ~data:structure ;
          Union_find.create id
      end

      let bottom (_state : t) =
        (* We don't actually store bottom shapes in the environment to gain some space. *)
        Private.create ()


      let variant_of_list constructor_list state =
        Private.create_and_store (Structure.variant_of_list constructor_list) state


      let local_abstract state = Private.create_and_store Structure.local_abstract state

      let vector_of_alist field_alist state =
        Private.create_and_store (Structure.vector_of_alist field_alist) state


      let vector_zero state = vector_of_alist [] state

      let vector_fully_abstract state =
        Private.create_and_store Structure.vector_fully_abstract state


      let complex_map ~all_map_value state =
        Private.create_and_store (Structure.complex_map ~all_map_value) state


      let scalar state = Private.create_and_store Structure.scalar state

      let as_variant {shape_structures; _} shape =
        let id = Union_find.get shape in
        match Hashtbl.find shape_structures id with
        | Some (Variant constructors) ->
            Some (String.Set.to_list constructors)
        | _ ->
            None


      (** Returns the shape of the asked field ([Ok]-wrapped) if it is known. If it isn't, returns
          [Error] with a boolean indicating if that shape should be considered as *fully* abstract
          (because the parent structure is) or not. *)
      let get_field {shape_structures; _} shape field_label : (shape, bool) result =
        let id = Union_find.get shape in
        match Hashtbl.find shape_structures id with
        | Some (Vector {fields; is_fully_abstract; _}) ->
            Map.find fields field_label |> Result.of_option ~error:is_fully_abstract
        | Some Bottom | Some (Variant _) | Some LocalAbstract | None ->
            Error false


      let get_all_map_value {shape_structures; _} shape =
        let id = Union_find.get shape in
        match Hashtbl.find shape_structures id with
        | Some (Vector {all_map_value; _}) ->
            all_map_value
        | _ ->
            None
    end

    (** Unify two structures and return the unified result. The sub-shapes that shall be unified
        will be put in the [todo] stack. *)
    let unify_structures (structure : Structure.t) (structure' : Structure.t) todo : Structure.t =
      let todo_unify shape shape' = Stack.push todo (shape, shape') in
      match (structure, structure') with
      | Bottom, other | other, Bottom ->
          other
      | Variant constructors, Variant constructors' ->
          Structure.variant (Set.union constructors constructors')
      | Variant _, other | other, Variant _ ->
          other
      | LocalAbstract, LocalAbstract ->
          Structure.local_abstract
      | LocalAbstract, other | other, LocalAbstract ->
          other
      | ( Vector
            { is_fully_abstract= is_fully_abstract_1
            ; all_map_value= all_map_value_1
            ; fields= fields_1 }
        , Vector
            { is_fully_abstract= is_fully_abstract_2
            ; all_map_value= all_map_value_2
            ; fields= fields_2 } ) ->
          (* Merge the fields of the second structure into the first one. During this merge, if we
             encounter a field present in both shapes, we put the shapes of this field in the todo
             stack to unify them at a later step (when the field table of the current processed
             shapes will be completed).
          *)
          let fields =
            Map.merge_skewed
              ~combine:(fun ~key:_fieldname shape_1 shape_2 ->
                todo_unify shape_1 shape_2 ;
                shape_1 )
              fields_1 fields_2
          in
          let is_fully_abstract = is_fully_abstract_1 || is_fully_abstract_2 in
          (* Compute all_map_value *)
          let all_map_value =
            match (all_map_value_1, all_map_value_2) with
            | None, other | other, None ->
                other
            | Some all_map_value_shape_1, Some all_map_value_shape_2 ->
                todo_unify all_map_value_shape_1 all_map_value_shape_2 ;
                Some all_map_value_shape_1
          in
          (* Unify map fields with all_map_value if it exists *)
          let () =
            match all_map_value with
            | None ->
                ()
            | Some all_map_value_shape ->
                Map.iteri
                  ~f:(fun ~key ~data ->
                    match (key : FieldLabel.t) with
                    | MapKey _ ->
                        todo_unify data all_map_value_shape
                    | Fieldname _ ->
                        () )
                  fields
          in
          (* Return the unified shape *)
          Structure.vector ~is_fully_abstract ?all_map_value fields


    let unify_step shape_structures shape shape' todo =
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
        match (Hashtbl.find shape_structures id, Hashtbl.find shape_structures id') with
        | None, None ->
            (* No subfield to unify *)
            ()
        | Some _, None ->
            (* Only one shape has a non-bottom structure, just use it as the new representative *)
            Union_find.set shape id
        | None, Some _ ->
            Union_find.set shape id'
        | Some structure, Some structure' ->
            (* Both shapes have a structure. We arbitrarily use the first id as a representative then
               associate to it the unified structures. The old structure associated to the now-unused
               second id can then be deleted. *)
            Union_find.set shape id ;
            let unified_structure = unify_structures structure structure' todo in
            (* Remark: we first remove then set in case id and id' are the same. This cannot happen
               on the current state of the code due to the [same_class] test above but is likely
               safer wrt. later potential code modifications. *)
            Hashtbl.remove shape_structures id' ;
            Hashtbl.set shape_structures ~key:id ~data:unified_structure


    (* Repeat the unification steps until the stack is empty. *)
    let rec unify_stack shape_structures todo =
      match Stack.pop todo with
      | None ->
          ()
      | Some (shape, shape') ->
          unify_step shape_structures shape shape' todo ;
          unify_stack shape_structures todo


    let unify {shape_structures; _} shape shape' =
      unify_stack shape_structures (Stack.singleton (shape, shape'))


    let unify_and_return state shape shape' =
      (* Useful interface to eg. List.reduce a shape list to unify it *)
      unify state shape shape' ;
      shape


    let shape_var ({var_shapes; _} as state) var =
      Hashtbl.find_or_add ~default:(fun () -> Shape.local_abstract state) var_shapes var


    let unify_var ({var_shapes; _} as state) var shape =
      let var_shape = Hashtbl.find_or_add ~default:(fun () -> Shape.bottom state) var_shapes var in
      unify state var_shape shape


    let unify_field state shape ~field_label ~value_shape =
      let vector_shape = Shape.vector_of_alist [(field_label, value_shape)] state in
      unify state shape vector_shape


    (** Returns the shape of a field of a structure if it exists, otherwise create and return it,
        either as a locally abstract shape or a fully abstract one depending on the parent
        structure. *)
    let shape_field state shape field_label =
      match Shape.get_field state shape field_label with
      | Ok value_shape ->
          (* Known field => return its shape *)
          value_shape
      | Error is_fully_abstract ->
          (* Unknown field => create a new one and return that shape, fully or locally abstract
             depending on the [get_field]-returned information. *)
          let value_shape =
            if is_fully_abstract then Shape.vector_fully_abstract state
            else Shape.local_abstract state
          in
          unify_field state shape ~field_label ~value_shape ;
          value_shape


    (** [shape_field] on record fields. *)
    let shape_record_field state shape fieldname =
      shape_field state shape (FieldLabel.fieldname fieldname)


    (** Unify the [all_map_value] component and all map-key-associated fields of [map_shape] with
        [value_shape]. [map_shape] will be incidentally unified with a structure shape if it wasn't
        already. *)
    let unify_all_map_value state ~map_shape ~value_shape =
      (* We rely on the unification to also set all the map-key fields of [map_shape]. *)
      let complex_map_shape = Shape.complex_map state ~all_map_value:value_shape in
      unify state map_shape complex_map_shape


    (** Returns the [all_map_value] component of a structure if it exists, otherwise create, set and
        return it as a vector shape. In the process, the argument shape will be unified with a
        vector if it wasn't already, and all its map key fields will be unified with [all_map_value]
        if they weren't. *)
    let shape_all_map_value state map_shape =
      match Shape.get_all_map_value state map_shape with
      | Some all_value_shape ->
          (* [all_map_value] exists: by invariant it is already unified with the map key fields. We
             can therefore simply return it. *)
          all_value_shape
      | None ->
          (* Create a new vector shape, unify it with the [all_map_value] component and every map key
             field and return it. *)
          let value_shape = Shape.vector_zero state in
          unify_all_map_value state ~map_shape ~value_shape ;
          value_shape


    (** Exported, see interface doc. *)
    let unify_map_value state ~map_shape ~key_shape ~value_shape =
      (* Defensively make sure that the map is vector-shaped. *)
      unify state map_shape (Shape.vector_zero state) ;
      match Shape.as_variant state key_shape with
      | Some constructors ->
          (* Key is a variant: we unify all the corresponding fields of the map argument. If
             [all_map_value] exists, then it is already unified with those fields: it will therefore
             also be affected in the process. If it doesn't we don't need to create it. *)
          List.iter
            ~f:(fun constructor ->
              unify_field state map_shape ~field_label:(FieldLabel.map_key constructor) ~value_shape
              )
            constructors
      | None ->
          (* Key isn't variant-shaped: we potentially unify every map value field in the map. *)
          unify_all_map_value state ~map_shape ~value_shape


    (** Exported, see interface doc. *)
    let shape_map_value state ~map_shape ~key_shape =
      (* Defensively make sure that the map is vector-shaped. *)
      unify state map_shape (Shape.vector_zero state) ;
      match Shape.as_variant state key_shape with
      | Some constructors ->
          (* Key is a variant: we discover the shape of the corresponding fields of the map argument
             and return their unified shape. [shape_field] shall take care of creating a suitable
             shape for unknown-yet fields. *)
          let value_shapes =
            List.map
              ~f:(fun constructor -> shape_field state map_shape (FieldLabel.map_key constructor))
              constructors
          in
          List.reduce_exn ~f:(unify_and_return state) value_shapes
      | None ->
          (* Key isn't variant-shaped: we potentially access every map values from the map. *)
          shape_all_map_value state map_shape
  end

  module Summary = struct
    (* A summary is similar to the (final) typing state environment of a function. The difference is
       that it "freezes" the union-find classes into some fixed and marshallable values. *)

    module Shape_class = struct
      (** Once the analysis is done, each class can be frozen into its representative. Therefore
          summary classes are simply shape ids, which trades off the now-uneeded mergeability for
          marshallability. *)

      type 'id t = 'id

      let pp pp_id = pp_id
    end

    include Types.Make (Shape_class) ()

    let find_var_shape var_shapes var =
      match Hashtbl.find var_shapes var with
      | Some shape ->
          shape
      | None ->
          L.die InternalError "No shape found for var %a" Var.pp var


    (** Returns true iff the corresponding shape would be represented by several cells in a fully
        precise abstraction, ie. has known sub-fields. *)
    let has_sub_cells shape_structures shape =
      match (Hashtbl.find shape_structures shape : Structure.t option) with
      | None | Some Bottom | Some (Variant _) | Some LocalAbstract ->
          false
      | Some (Vector {all_map_value= Some _; _}) ->
          true
      | Some (Vector {all_map_value= None; fields}) ->
          (* 1-sized vectors are considered to have sub-cells. This is sometimes an approximation but
             avoids recursing on that lone field to check if it itself has more than one
             sub-cells. *)
          not (Map.is_empty fields)


    let find_field_table shape_structures shape =
      match Hashtbl.find shape_structures shape with
      | Some (Structure.Vector {fields; _}) ->
          fields
      | other ->
          L.die InternalError "No field table found for shape %a = %a" Shape_id.pp shape
            (Fmt.option Structure.pp) other


    let find_next_field_shape shape_structures shape field =
      let field_table = find_field_table shape_structures shape in
      match Map.find field_table field with
      | Some value_shape ->
          value_shape
      | None ->
          L.die InternalError "Field %a unknown for shape %a.@ Known fields are:@ @[{%a}@]"
            FieldLabel.pp field Shape_id.pp shape
            (pp_map ~bind:IFmt.colon_sp FieldLabel.pp Shape_id.pp)
            field_table


    let find_var_path_shape {var_shapes; shape_structures} var field_path =
      let var_shape = find_var_shape var_shapes var in
      List.fold
        ~f:(fun shape field -> find_next_field_shape shape_structures shape field)
        ~init:var_shape field_path


    let make {State.var_shapes; shape_structures} =
      (* Making a summary from a state essentially amounts to freezing State shape classes into State
         ids and converting those State ids into Summary ids. We keep an id translation table that
         maps state ids into summary ids and generate a fresh summary id whenever we encounter a new
         state id representative. *)
      let id_translation_tbl = Hashtbl.create (module State.Shape_id) in
      let translate_shape_id state_shape_id =
        Hashtbl.find_or_add id_translation_tbl ~default:Shape_id.create state_shape_id
      in
      let translate_shape state_shape = translate_shape_id (Union_find.get state_shape) in
      let translate_structure : State.Structure.t -> Structure.t = function
        | Bottom ->
            Structure.bottom
        | LocalAbstract ->
            (* It's likely possible to not have locally abstract structures appear in summaries
               altogether, but that makes debugging easier. *)
            Structure.local_abstract
        | Variant constructors ->
            Structure.variant constructors
        | Vector {is_fully_abstract; all_map_value; fields} ->
            let all_map_value = Option.map ~f:translate_shape all_map_value in
            Structure.vector ~is_fully_abstract ?all_map_value (Map.map ~f:translate_shape fields)
      in
      let var_shapes = Hashtbl.map ~f:translate_shape var_shapes in
      let shape_structures =
        iter_hashtbl shape_structures
        |> Iter.map2 (fun shape structure ->
               (translate_shape_id shape, translate_structure structure) )
        |> hashtbl_of_iter_exn (module Shape_id)
      in
      {var_shapes; shape_structures}


    (* Introducing a (callee) summary is not as simple as freezing an environment into a summary,
       because the summary also contains shape fields of all the local variables of the callee that
       we do not want to put into the caller environment. Therefore we proceed by only introducing
       the shapes of some explicit variables (that will be the formals and return of the callee), and
       recursively discovering and introducing their fields. *)
    let rec introduce_shape id_translation_tbl shape_id shape_structures state_shape_structures =
      (* Translate and introduce a shape from the summary into the environment shapes *)
      match Hashtbl.find id_translation_tbl shape_id with
      | Some state_shape ->
          (* If the shape is already present in the table, then it has already been introduced
             earlier. Just return its translation. *)
          state_shape
      | None ->
          (* This is a new shape to translate. Create a fresh environment shape and populate it by
             recursively introducing its fields. *)
          let state_shape = State.Shape.Private.create () in
          Hashtbl.set id_translation_tbl ~key:shape_id ~data:state_shape ;
          ( match (Hashtbl.find shape_structures shape_id : Structure.t option) with
          | None
          | Some Bottom
          | Some LocalAbstract
          (* Locally abstract shapes from the callee are not introduced in the caller to allow
             their unification with concrete arguments (which might themselves be locally
             abstract within the caller). *) ->
              ()
          | Some (Variant constructors) ->
              Hashtbl.set state_shape_structures ~key:(Union_find.get state_shape)
                ~data:(State.Structure.variant constructors)
          | Some (Vector {is_fully_abstract; all_map_value; fields}) ->
              let all_map_value =
                match all_map_value with
                | None ->
                    None
                | Some all_map_value_id ->
                    Some
                      (introduce_shape id_translation_tbl all_map_value_id shape_structures
                         state_shape_structures )
              in
              Hashtbl.set state_shape_structures ~key:(Union_find.get state_shape)
                ~data:
                  (State.Structure.vector ~is_fully_abstract ?all_map_value
                     (introduce_fields id_translation_tbl fields shape_structures
                        state_shape_structures ) ) ) ;
          state_shape


    and introduce_fields id_translation_tbl fields shape_structures state_shape_structures =
      Map.map
        ~f:(fun shape_id ->
          introduce_shape id_translation_tbl shape_id shape_structures state_shape_structures )
        fields


    let introduce_var ~var id_translation_tbl {var_shapes; shape_structures}
        {State.shape_structures= state_shape_structures; _} =
      introduce_shape id_translation_tbl (Hashtbl.find_exn var_shapes var) shape_structures
        state_shape_structures


    let introduce ~formals ~return summary state =
      (* [id_translation_tbl] maps Ids from the summary to their translation as Ids in the state
         environment of the caller function. *)
      let id_translation_tbl = Hashtbl.create (module Shape_id) in
      (* We introduce into the (caller) state the *formal* parameters and return value from the
         (callee) summary. It will be the responsibility of the call-interpretation code to then
         unify these formals with the actual parameters and ret_id that already live in the caller
         state. *)
      let args_state_shapes =
        List.map ~f:(fun arg -> introduce_var ~var:arg id_translation_tbl summary state) formals
      in
      let return_state_shape = introduce_var ~var:return id_translation_tbl summary state in
      (args_state_shapes, return_state_shape)


    (** Boxing fields are fields that are internally generated by the frontend to box scalar values,
        such as integer or atoms. They should not be considered as actual fields by the Lineage
        analysis. *)
    let is_boxing_field fieldname =
      Array.mem ~equal:FieldLabel.equal
        [| FieldLabel.make_fieldname (ErlangType Atom) ErlangTypeName.atom_name
         ; FieldLabel.make_fieldname (ErlangType Atom) ErlangTypeName.atom_hash
         ; FieldLabel.make_fieldname (ErlangType Integer) ErlangTypeName.integer_value |]
        fieldname


    let finalise {var_shapes; shape_structures} var field_path =
      let rec aux remaining_depth traversed_shape_set shape field_path : FieldPath.t * bool =
        match field_path with
        (* Walk through the fields and ensure that we do not:
           - Traverse a field table wider than LineageConfig.field_width
           - Traverse cycles if forbidden by the option
           - "Traverse" (they must be in last position) internal "boxing" fields that should be
             ignored
           Also limit the depth by stopping the traversal once the remaining depth reaches zero.

           Returns the final field path and a boolean indicating if the corresponding cell is
           abstract; that is, it semantically has some fields but we forget about them because we hit
           one of the aforementioned limits.
        *)
        | [] ->
            ([], has_sub_cells shape_structures shape)
        | [field] when is_boxing_field field ->
            ([], false)
        | field :: _ when is_boxing_field field ->
            L.die InternalError "LineageShape: unexpected boxing field in non tail position."
        | field :: fields ->
            let field_table = find_field_table shape_structures shape in
            if
              Int.(remaining_depth <= 0)
              || Map.length field_table > LineageConfig.field_width
              || (LineageConfig.prevent_cycles && Set.mem traversed_shape_set shape)
            then ([], true)
            else
              let final_sub_path, is_abstract =
                aux (remaining_depth - 1)
                  (Set.add traversed_shape_set shape)
                  (Map.find_exn field_table field) fields
              in
              (field :: final_sub_path, is_abstract)
      in
      let var_shape = find_var_shape var_shapes var in
      let field_path, is_abstract =
        aux LineageConfig.field_depth (Set.empty (module Shape_id)) var_shape field_path
      in
      Cell.create ~var ~field_path ~is_abstract


    (** Given field shapes, a particular shape, a maximal width, a maximal search depth and a
        boolean indicating that cycles should not be traversed, traverses the field shapes table and
        builds the field paths obtained by recursively adding all the defined fieldnames of the
        considered shape to the prefixes.

        The traversal building a field path will stop when the maximal search depth has been
        reached, a shape with more than [LineageConfig.field_width] fields is encountered, or a
        shape that has already been traversed is encountered and [LineageConfig.prevent_cycles] is
        true.

        When the building of a field path stops, the folding function [f] will be called on the path
        built so far.

        Note that the traversal and the calling of [f] is done from the original argument shape: for
        instance, if [X] has the field [X#foo#bar#baz], the depth limit is 2 and this function is
        called from the shape of [X#foo], then [f] will be called with the field path [bar#baz] --
        even if [X#foo] and [X#bar#baz] have the same shape, or [X] has a thousand other fields. For
        this reason, [~max_search_depth] is an explicit parameter, different from the
        {!LineageConfig.field_depth} configuration option.

        The typical use-case is to then have [f] call {!finalise}, with [X#foo] in the parameters,
        which will repeat a similar traversal to only yield [X#foo#bar] as a final field path. This
        allows implementing {!fold_cell_pairs} by first getting the candidate final fields from a
        common shape of two different origin paths, then finalising separately wrt. these two paths
        (which could have different depths to begin with). *)
    let fold_final_fields_of_shape shape_structures shape ~search_depth ~init ~f =
      let rec aux shape depth traversed field_path_acc ~init =
        if Int.(depth >= search_depth) || (LineageConfig.prevent_cycles && Set.mem traversed shape)
        then f init (List.rev field_path_acc)
        else
          match (Hashtbl.find shape_structures shape : Structure.t option) with
          | None | Some Bottom | Some (Variant _) | Some LocalAbstract ->
              (* No more fields. *)
              f init (List.rev field_path_acc)
          | Some (Vector {fields; _}) ->
              let len = Map.length fields in
              if Int.(len = 0 || len > LineageConfig.field_width) then
                f init (List.rev field_path_acc)
              else
                let traversed = Set.add traversed shape in
                Map.fold
                  ~f:(fun ~key:fieldname ~data:fieldshape acc ->
                    aux fieldshape (depth + 1) traversed (fieldname :: field_path_acc) ~init:acc )
                  fields ~init
      in
      aux shape 0 (Set.empty (module Shape_id)) [] ~init


    let fold_cells {var_shapes; shape_structures} (var, field_path) ~init ~f =
      let var_path_shape = find_var_path_shape {var_shapes; shape_structures} var field_path in
      let search_depth = LineageConfig.field_depth - List.length field_path in
      fold_final_fields_of_shape shape_structures var_path_shape ~search_depth ~init
        ~f:(fun acc sub_path ->
          f acc (finalise {var_shapes; shape_structures} var (field_path @ sub_path)) )


    let fold_cell_pairs {var_shapes; shape_structures} (var_1, field_path_1) (var_2, field_path_2)
        ~init ~f =
      let var_path_shape_1 =
        find_var_path_shape {var_shapes; shape_structures} var_1 field_path_1
      in
      let var_path_shape_2 =
        find_var_path_shape {var_shapes; shape_structures} var_2 field_path_2
      in
      if not ([%equal: Shape_id.t] var_path_shape_1 var_path_shape_2) then
        L.die InternalError
          "@[Attempting to get related fields of differently shaped fields: @[%a={%a}@]@ vs@ \
           @[%a={%a}@]@]"
          Shape_id.pp var_path_shape_1 (Fmt.option Structure.pp)
          (Hashtbl.find shape_structures var_path_shape_1)
          Shape_id.pp var_path_shape_2 (Fmt.option Structure.pp)
          (Hashtbl.find shape_structures var_path_shape_2)
      else
        let search_depth =
          (* Use the shallowest argument to determine the search depth. *)
          LineageConfig.field_depth - Int.min (List.length field_path_1) (List.length field_path_2)
        in
        fold_final_fields_of_shape shape_structures var_path_shape_1 ~search_depth ~init
          ~f:(fun acc sub_path ->
            f acc
              (finalise {var_shapes; shape_structures} var_1 (field_path_1 @ sub_path))
              (finalise {var_shapes; shape_structures} var_2 (field_path_2 @ sub_path)) )
  end
end

(** As the state works imperatively, we do not need to propagate it and therefore use a simple
    Top/Bottom abstract domain that will simply remember which nodes have been reached yet. Note
    that the resulting analysis is flow insensitive, as the same global shape state will be
    augmented during the traversal of a procedure code, and no intermediary version of it will be
    associated to the individual control flow vertices. *)
module Domain = AbstractDomain.Unit

module Summary = Env.Summary

module Report = struct
  (** Reporting utility module. *)

  let debug proc_desc summary =
    (* Print a summary and the fields of the returned value in the debug logs. *)
    let procname = Procdesc.get_proc_name proc_desc in
    L.debug Analysis Verbose "@[<v>@ @[<v2>" ;
    L.debug Analysis Verbose "@[<v>Result for procedure : %a@]@ " Procname.pp procname ;
    L.debug Analysis Verbose "@[<v2>SUMMARY:@ %a@]@ @ " Env.Summary.pp summary ;
    L.debug Analysis Verbose "@[<v2>FIELDS OF RETURN:@ (%a)@]"
      (Fmt.iter
         (fun f summary ->
           Env.Summary.fold_cells summary
             (Var.of_pvar (Procdesc.get_ret_var proc_desc), [])
             ~f:(fun () fields -> f fields)
             ~init:() )
         ~sep:Fmt.comma Cell.pp )
      summary ;
    L.debug Analysis Verbose "@]@ @]"
end

(** Transfer functions to compute shapes. As the state is an imperative structure, this module takes
    a global state as a parameter that will be mutated through the analysis by the hereby defined
    transfer functions. *)
module TransferFunctions (State : sig
  val state : Env.State.t
end) =
struct
  module Domain = Domain
  module CFG = CFG

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let state = State.state

  (** Returns the shape of an expression. Fresh shapes will be created as needed. *)
  let rec shape_expr (e : Exp.t) =
    match e with
    | Const _ | Closure _ ->
        Env.State.Shape.scalar state
    | Var id ->
        Env.State.shape_var state (Var.of_id id)
    | Lvar pvar ->
        Env.State.shape_var state (Var.of_pvar pvar)
    | Lfield (e, fieldname, _) ->
        let shape_e = shape_expr e in
        Env.State.shape_record_field state shape_e fieldname
    | Sizeof {dynamic_length= None} ->
        Env.State.Shape.scalar state
    | UnOp (_, e, _) | Exn e | Cast (_, e) | Sizeof {dynamic_length= Some e} ->
        (* We first shape [e] to possibly discover some fields (eg. on [not (x.f)]), then return
           a scalar shape as unary operators only return scalar value. *)
        ignore (shape_expr e : Env.State.shape) ;
        Env.State.Shape.scalar state
    | BinOp (_, e1, e2) | Lindex (e1, e2) ->
        (* Similar to the UnOp case *)
        ignore (shape_expr e1 : Env.State.shape) ;
        ignore (shape_expr e2 : Env.State.shape) ;
        Env.State.Shape.scalar state


  module CallModel = struct
    let make_atom ret_id args =
      match args with
      | [Exp.Const (Cstr name); _hash_exp] ->
          let variant_shape = Env.State.Shape.variant_of_list [name] state in
          Env.State.unify_var state ret_id variant_shape
      | _ ->
          L.die InternalError
            "make_atom should have two arguments, the first one being a constant string."


    let make_tuple ret_id args =
      (* Unify the shape of the return with a Vector made from the arguments *)
      let tuple_type : Typ.name = ErlangType (Tuple (List.length args)) in
      let fieldname i = FieldLabel.make_fieldname tuple_type (ErlangTypeName.tuple_elem (i + 1)) in
      let arg_vector_shape =
        Env.State.Shape.vector_of_alist
          (List.mapi ~f:(fun i arg -> (fieldname i, shape_expr arg)) args)
          state
      in
      Env.State.unify_var state ret_id arg_vector_shape


    let maps_put ret_id args =
      (* Access the key and return the map. *)
      match args with
      | [key_exp; value_exp; map_exp] ->
          let key_shape = shape_expr key_exp in
          let value_shape = shape_expr value_exp in
          let map_shape = shape_expr map_exp in
          Env.State.unify_map_value state ~map_shape ~key_shape ~value_shape ;
          Env.State.unify_var state ret_id map_shape
      | _ ->
          L.die InternalError "`maps:put` expects three arguments"


    let maps_get ret_id args =
      (* Access the key and return the value. *)
      match args with
      | [key_exp; map_exp] ->
          let key_shape = shape_expr key_exp in
          let map_shape = shape_expr map_exp in
          let value_shape = Env.State.shape_map_value state ~map_shape ~key_shape in
          Env.State.unify_var state ret_id value_shape
      | _ ->
          L.die InternalError "`maps:get` expects two arguments"


    let maps_new ret_id args =
      (* Simply return an empty vector. Having a specific model will prevent unknown-procedure warnings. *)
      match args with
      | [] ->
          let map_shape = Env.State.Shape.vector_zero state in
          Env.State.unify_var state ret_id map_shape
      | _ ->
          L.die InternalError "`maps:new` expects zero argument"


    let make_map ret_id args =
      (* Create an empty map and unify all keys. Return the map. *)
      let map_shape = Env.State.Shape.vector_zero state in
      let args = List.chunks_of ~length:2 args in
      List.iter
        ~f:(function
          | [key_exp; value_exp] ->
              let key_shape = shape_expr key_exp in
              let value_shape = shape_expr value_exp in
              Env.State.unify_map_value state ~map_shape ~key_shape ~value_shape
          | _ ->
              L.die InternalError "make_map expects an even number of arguments" )
        args ;
      Env.State.unify_var state ret_id map_shape


    let get_custom_model procname =
      let models =
        [ (BuiltinDecl.__erlang_make_tuple, make_tuple)
        ; (BuiltinDecl.__erlang_make_atom, make_atom)
        ; (BuiltinDecl.__erlang_make_map, make_map)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"new" ~arity:0, maps_new)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"get" ~arity:2, maps_get)
        ; (Procname.make_erlang ~module_name:"maps" ~function_name:"put" ~arity:3, maps_put) ]
      in
      List.Assoc.find ~equal:Procname.equal models procname


    let unknown_model ~pp_callee ~callee ret_var args =
      L.debug Analysis Verbose "@[<v2> LineageShape: no model found for procname `%a`@]@," pp_callee
        callee ;
      (* As a best effort, we unify the return with the most general shape of a fully abstract
         vector, with no known field yet. This might not really support unknown functions that return
         complex maps, or stateful functions (whose returned type could be unified between
         invocations). This might be an issue for a proper type system, but it is most likely fine
         for the Lineage purpose. *)
      let ret_shape = Env.State.Shape.vector_fully_abstract state in
      Env.State.unify_var state ret_var ret_shape ;
      (* Shape the arguments to make sure the analysis is aware of them *)
      ignore (List.map ~f:shape_expr args : Env.State.shape list) ;
      ()


    let unknown_exp exp ret_var args =
      unknown_model
        ~pp_callee:Fmt.(any "expression " ++ quote ~mark:"`" Exp.pp)
        ~callee:exp ret_var args


    let unknown_procname procname ret_var args =
      unknown_model
        ~pp_callee:Fmt.(any "procname" ++ quote ~mark:"`" Procname.pp)
        ~callee:procname ret_var args


    let standard_model procname summary ret_var args =
      (* Standard call of a known function:
         1. We get the shape of the actual args
         2. We introduce into the environment the shapes of the formal args and return value of
            the function, obtained from the summary.
         3. We unify the actual and formal params together.
         4. We unify the ret_id shape with the formal return shape. Due to the previous step it will
            therefore correctly be related to the shapes of the actual parameters of the function.
      *)
      let actual_args_shapes = List.map ~f:shape_expr args in
      let return = Var.of_pvar (Pvar.get_ret_pvar procname) in
      let formals =
        Attributes.load_exn procname |> ProcAttributes.get_pvar_formals
        |> List.map ~f:(fun (pvar, _typ) -> Var.of_pvar pvar)
      in
      let formal_shapes, returned_shape = Env.Summary.introduce ~return ~formals summary state in
      List.iter2_exn ~f:(fun s1 s2 -> Env.State.unify state s1 s2) actual_args_shapes formal_shapes ;
      Env.State.unify_var state ret_var returned_shape


    let exec analyze_dependency procname ret_var args =
      match get_custom_model procname with
      | Some model ->
          model ret_var args
      | None -> (
        match analyze_dependency procname with
        | Some summary ->
            standard_model procname summary ret_var args
        | None ->
            unknown_procname procname ret_var args )
  end

  let exec_assignment var rhs_exp =
    (* When assigning a value to a variable, we unify the current shape of that variable to the shape
       of the expression, thus merging together the fields that have been collected so far on both
       sides.

       Note that this might lead to over-approximating the field set of a variable that would be
       reassigned in the program with completely unrelated types. We believe that it does not happen
       with the Erlang translation anyway (and even then would not be a fundamental issue). *)
    let expr_shape = shape_expr rhs_exp in
    Env.State.unify_var state var expr_shape


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
            CallModel.unknown_exp fun_exp ret_var args
        | Some procname ->
            CallModel.exec analyze_dependency procname ret_var args )
    | Prune (e, _, _, _) ->
        ignore (shape_expr e : Env.State.shape)
    | Metadata _ ->
        ()
    | Load {id; e; _} ->
        exec_assignment (Var.of_id id) e
    | Store {e1= Lvar pvar; e2; _} ->
        (* Same as Load *)
        exec_assignment (Var.of_pvar pvar) e2
    | Store _ ->
        L.die InternalError "LineageShape: Store instructions are only supported with Lvar lhs"


  (** Mutates the environment and then return an abstract state (which is actually the same as the
      parameter). *)
  let exec_instr astate interproc_data _node _instr_index instr =
    let () = exec_instr_unit interproc_data instr in
    astate


  let pp_session_name _node fmt = Format.pp_print_string fmt "LineageShape"
end

(** A generative module that creates a fresh environment and passes it to the {!TransferFunctions}
    functor to build an analysis engine. *)
module Analyzer () = struct
  module State = struct
    let state = Env.State.create ()
  end

  include AbstractInterpreter.MakeWTO (TransferFunctions (State))
end

let unskipped_checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let module Analyzer = Analyzer () in
  (* Shape captured vars *)
  let shape_captured_var {CapturedVar.pvar} =
    ignore (Env.State.shape_var Analyzer.State.state (Var.of_pvar pvar) : Env.State.shape)
  in
  List.iter ~f:shape_captured_var (Procdesc.get_captured proc_desc) ;
  (* Analyze the procedure's code  *)
  let _invmap : Analyzer.invariant_map = Analyzer.exec_pdesc analysis_data ~initial:() proc_desc in
  let summary = Env.Summary.make Analyzer.State.state in
  Report.debug proc_desc summary ;
  Some summary


let checker analysis_data =
  (* We skip the functions that would not be analysed by Lineage anyway *)
  LineageUtils.skip_unwanted (fun data () -> unskipped_checker data) analysis_data ()
