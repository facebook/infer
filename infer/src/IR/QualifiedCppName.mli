(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t [@@deriving compare]

val empty : t
(** empty qualified name *)

val equal : t -> t -> bool

val of_qual_string : string -> t
(** attempts to parse the argument into a list::of::possibly::templated<T>::qualifiers *)

val to_qual_string : t -> string
(** returns qualified name as a string with "::" as a separator between qualifiers *)

val append_qualifier : t -> qual:string -> t
(** append qualifier to the end (innermost scope) of the qualified name *)

val extract_last : t -> (string * t) option
(** returns last (innermost scope) qualifier and qualified name without last qualifier *)

val strip_template_args : t -> t
(** returns qualified name without template arguments. For example:
    input: std::shared_ptr<int>::shared_ptr<long>
    output: std::shared_ptr::shared_ptr *)

val append_template_args_to_last : t -> args:string -> t
(** append template arguments to the last qualifier. Fails if qualified name is empty or it already has
    template args *)

val to_list : t -> string list
(** returns list of qualifers *)

val to_rev_list : t -> string list
(** returns reversed list of qualifiers, ie innermost scope is the first element *)

val of_list : string list -> t
(** given list of qualifiers in normal order produce qualified name ["std", "move"] *)

val of_rev_list : string list -> t
(** given reversed list of qualifiers, produce qualified name (ie. ["move", "std"] for std::move )*)

val pp : Format.formatter -> t -> unit

(** Module to match qualified C++ procnames "fuzzily", that is up to namescapes and templating. In
    particular, this deals with the following issues:

    1. 'std::' namespace may have inline namespace afterwards: std::move becomes std::__1::move. This
        happens on libc++ and to some extent on libstdc++. To work around this problem, make matching
        against 'std::' more fuzzier: std::X::Y::Z will match std::.*::X::Y::Z (but only for the
        'std' namespace).

    2. The names are allowed not to commit to a template specialization: we want std::move to match
       std::__1::move<const X&> and std::__1::move<int>. To do so, comparison function for qualifiers
       will ignore template specializations.

       For example:
       ["std", "move"]:
       matches: ["std", "blah", "move"]
       matches: ["std", "blah<int>", "move"]
       does not match: ["std","blah", "move", "BAD"] - we don't want std::.*::X::.* to pass
       does not match: ["stdBAD", "move"], - it's not std namespace anymore

       ["folly", "someFunction"]
       matches: ["folly","someFunction"]
       matches: ["folly","someFunction<int>"]
       matches: ["folly<int>","someFunction"]
       does not match: ["folly", "BAD", "someFunction"] - unlike 'std' any other namespace needs all
                                                          qualifiers to match
       does not match: ["folly","someFunction<int>", "BAD"] - same as previous example
   *)
module Match : sig
  type quals_matcher

  val of_fuzzy_qual_names : string list -> quals_matcher

  val match_qualifiers : quals_matcher -> t -> bool
end
