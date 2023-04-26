(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Fields : sig
  type t = Fieldname.t list [@@deriving compare, equal, sexp, yojson_of]

  val pp : t Fmt.t
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val fold_terminal_fields :
       t
    -> Var.t * Fields.t
    -> max_width:int
    -> max_depth:int
    -> prevent_cycles:bool
    -> init:'accum
    -> f:('accum -> Fields.t -> 'accum)
    -> 'accum
  (** Folds over all terminal fields of a variable with fields. A field list is "terminal" if its
      length (that includes the prefixed fields given as parameters) is equal to [max_depth], or no
      more field can be subscripted from its corresponding type, or it has strictly more than
      [max_width] immediate subfields.

      The parameter fields are expected in syntactic order: [\[a; b\]] for [x#a#b].

      The result will not cross any shape whose field table is wider than [max_width], even if one
      of the parameter does. For instance, if some variable [X] has a huge number of fields, the
      only terminal field of [X#field1] will be [X] itself. *)

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
  (** Folds over all terminal fields of two same-shape variables with their fields. See
      {!fold_terminal_fields}.

      Dies if the parameters do not have the same shape.

      The folding function [f] will always be called on pairs of terminal fields.

      Note that, since the parameters field lists may not have the same length, [f] may be called on
      field lists of different lengths. For instance, if the parameters [var#field1] and
      [var'#field2#field3] don't have any subfield, then [f] will be called with [#field1] and
      [#field2#field3].

      Also, since the parameters lengths are taken into account for determining the depth of the
      result, the arguments of [f] may not have the same suffix when extracting these parameters:
      for instance, if [max_depth = 1] and [var] and [var'#field] have a subfield [foo], then [f]
      will be called on [#foo] and [#field].

      The same width limitation as the function {!fold_terminal_fields} is ensured for both
      parameters (even if these parameters cross wide shapes, the result will not). *)
end

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
