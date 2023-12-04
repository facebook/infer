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
  (** The fields are listed in syntactic order: [\[a; b\]] for [x#a#b]. *)
  type t = FieldLabel.t list [@@deriving compare, equal, sexp, hash, yojson_of]

  include Comparable.S with type t := t

  val pp : t Fmt.t
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
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val fold_field_labels :
       t option
    -> Var.t * FieldPath.t
    -> init:'accum
    -> f:('accum -> FieldLabel.t -> 'accum)
    -> fallback:('accum -> 'accum)
    -> 'accum
  (** If a variable path has a shape that corresponds to a statically known set of field labels,
      folds over those field labels.

      Otherwise, calls the [fallback] function on [init].

      If the summary is [None], will always fallback. *)

  val fold_cells :
    t option -> Var.t * FieldPath.t -> init:'accum -> f:('accum -> Cell.t -> 'accum) -> 'accum
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

  val fold_cell_pairs :
       t option
    -> Var.t * FieldPath.t
    -> Var.t * FieldPath.t
    -> init:'accum
    -> f:('accum -> Cell.t -> Cell.t -> 'accum)
    -> 'accum
  (** Folds over all corresponding cell pairs of two same-shape variable and field paths. See
      {!fold_cells}.

      Dies if the parameters do not have the same shape.

      Note that, since the parameters field paths may not have the same length, [f] may be called on
      field paths of different lengths. For instance, if the parameters [var#field1] and
      [var'#field2#field3] don't have any subfield, then [f] will be called with [#field1] and
      [#field2#field3].

      Also, since the parameters lengths are taken into account for determining the depth of the
      result, the arguments of [f] may not have the same suffix when extracting these parameters:
      for instance, if [Config.lineage_field_depth = 1] and [var] and [var'#field] have a subfield
      [foo], then [f] will be called on [#foo] and [#field].

      The same width limitation as the function {!fold_cells} is ensured for both parameters (even
      if these parameters cross wide shapes, the result will not).

      If the summary is [None], [f] will be called once with a var-only abstract cells pair. *)
end

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
