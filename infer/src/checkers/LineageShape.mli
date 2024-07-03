(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module FieldLabel : sig
  type t [@@deriving compare, equal, sexp, yojson_of]

  val fieldname : Fieldname.t -> t

  val make_fieldname : Typ.name -> string -> t

  val tuple_elem_zero_based : size:int -> index:int -> t
end

module FieldPath : sig
  (** A module to help manipulating lists of (nested) fields. *)

  (** The fields are listed in syntactic order: [[a; b]] for [x#a#b]. *)
  type t = FieldLabel.t list [@@deriving compare, equal, sexp, hash, yojson_of]

  include Comparable.S with type t := t

  val pp : t Fmt.t

  val first_field_is : FieldLabel.t -> t -> bool
  (** Returns [true] iff the field path is not empty and its head is equal to the field label. *)
end

module VarPath : sig
  (** A variable path is a pair of a variable and a possibly empty list of subscripted fields. They
      are built from their in-program occurrences. They may semantically have sub-fields themselves:
      it is the job of the {!Cell} module to determine the final graph nodes constructed from paths. *)

  (** The type of variable paths: a variable and a possibly empty list of subscripted fields. *)
  type t = Var.t * FieldPath.t

  val var : Var.t -> t

  val sub_label : t -> FieldLabel.t -> t
  (** Subscript one sub-field from a variable path.*)

  val sub_path : t -> FieldPath.t -> t
  (** Subscript nested sub-fields from a variable path. *)

  val make : Var.t -> FieldPath.t -> t

  val pvar : Pvar.t -> t

  val ident : Ident.t -> t
end

module Cell : sig
  (** Cells correspond to variable fields for which no subfield will be considered by the analysis,
      either because they semantically have none, or because the abstract domain decides that
      considering them would lead to too deep or too wide field structures.

      A field path of a cell:

      - Cannot have a length greater than {!IBase.Config.lineage_field_depth}. For instance, if that
        limit is [2], the only cell under both [X#foo#bar#baz] and [X#foo#bar#ham] will be
        [X#foo#bar].

      - Cannot cross a field whose field table is wider than {!IBase.Config.lineage_field_width}.
        For instance, of the variable [X] has a huge number of fields, the only cell under
        [X#field1] will be [X] itself.

      - Cannot go twice on same-typed fields if {!IBase.Config.lineage_prevent_cycles} is set. For
        instance, if [X] has fields [X#head] and [X#tail], and [X#tail] has the same shape as [X],
        then the cells under [X] will be [X#head] and [X#tail].

      If one of these conditions happens, then the cell field path will be truncated before reaching
      a "scalar" field and the cell will be "abstract".

      To make sure that the aforementioned properties hold, one cannot construct cells directly, but
      should access them through the {!Summary.fold_cells} and {!Summary.fold_cell_pairs} functions.

      The lineage graph is built on cells. *)

  type t [@@deriving compare, equal, sexp, yojson_of, hash]

  val pp : t Fmt.t

  val var : t -> Var.t

  val field_path : t -> FieldPath.t

  val is_abstract : t -> bool

  val var_appears_in_source_code : t -> bool

  val path_from_origin : origin:VarPath.t -> t -> FieldPath.t
  (** Assuming the cell represents a component of the origin variable path, returns the sub-path
      subscriptable from this origin path to reach the cell component.

      Raises if the cell and the origin path are incompatible (eg. their variables are different or
      the cell path and the origin path are incomatible).

      If the cell is larger than the origin path (eg. because the origin path is longer than the
      limit and the cells groups it with other paths), returns an empty path. The rationale is that
      the result of this function can be seen as the subpath which, when extracted from the origin,
      will cover all the components of that origin stored in the cell. If the cell is larger than
      the origin then all the components of the origin are in the cell.

      Raises if the cell and the origin path are incompatible (eg. their variables are different).

      Examples assuming a depth limit of 3:

      - origin: X#a ; cell: X#a#b#c ; result : #b#c
      - origin: X#a#b#c ; cell : X#a#b#c ; result : empty
      - origin: X#a#b#c ; cell : X#a#b#d ; result : raises
      - origin: X#a#b#c ; cell : X#a#b#d ; result : raises
      - origin: X#a#b#c#d ; cell : X#a#b#c ; result : empty *)
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val assert_equal_shapes : t option -> VarPath.t -> VarPath.t -> unit

  val fold_field_labels :
       t option
    -> VarPath.t
    -> init:'accum
    -> f:('accum -> FieldLabel.t -> 'accum)
    -> fallback:('accum -> 'accum)
    -> 'accum
  (** If a variable path has a shape that corresponds to a statically known set of field labels,
      folds over those field labels.

      Otherwise, calls the [fallback] function on [init].

      If the summary is [None], will always fallback. *)

  val as_field_label_singleton : t option -> VarPath.t -> FieldLabel.t option
  (** If a variable path has the shape that corresponds to a single statically known label, return
      it. *)

  val fold_cells : t option -> VarPath.t -> init:'accum -> f:('accum -> Cell.t -> 'accum) -> 'accum
  (** Folds over all cells under a variable and field path. A field path is "terminal" if its length
      (that includes the prefixed fields given as parameters) is equal to
      {!IBase.Config.lineage_field_depth}, or no more field can be subscripted from its
      corresponding type, or it has strictly more than {!IBase.Config.lineage_field_width} immediate
      subfields.

      The result will not cross any shape whose field table is wider than
      {!IBase.Config.lineage_field_width}, even if one of the parameter does. For instance, if some
      variable [X] has a huge number of fields, the only terminal field of [X#field1] will be [X]
      itself.

      If the summary is [None], [f] will be called once with a var-only abstract cell. *)

  val fold_argument :
    t option -> int -> FieldPath.t -> init:'accum -> f:('accum -> FieldPath.t -> 'accum) -> 'accum
  (** Folds over the terminal field paths of the given field of the argument at the given index. See
      {!fold_cells}. *)

  val fold_return :
    t option -> FieldPath.t -> init:'accum -> f:('accum -> FieldPath.t -> 'accum) -> 'accum
  (** Folds over the terminal field paths of the given field of the formal return. See
      {!fold_cells}. *)

  (** Special instances of [fold_...] functions when one wants to build a list. *)

  val map_argument : t option -> int -> FieldPath.t -> f:(FieldPath.t -> 'a) -> 'a list

  val map_return : t option -> FieldPath.t -> f:(FieldPath.t -> 'a) -> 'a list

  val map_return_of : t option -> Procname.t -> FieldPath.t -> f:(FieldPath.t -> 'a) -> 'a list
end

module StdModules : sig
  (** Can be safely opened to provide module definitions at once. *)

  module FieldLabel = FieldLabel
  module FieldPath = FieldPath
  module VarPath = VarPath
  module Cell = Cell
end

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
