/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

type t = list string [@@deriving compare];

let equal = [%compare.equal : t];

let empty = [];

let append_qualifier quals qual::qual => List.append quals [qual];

let to_list quals => quals;

let of_list quals => quals;

/* This is simplistic and will give the wrong answer in some cases, eg
   "foo<bar::baz<goo>>::someMethod" will get parsed as ["foo<bar", "baz<goo>>",
   "someMethod"]. Ideally, we would keep the list of qualifiers in the procname, which would save us
   from having to properly parse them. */
let of_qual_string = {
  let class_sep_regex = Str.regexp_string "::";
  /* wait until here to define the function so that [class_sep_regex] is only computed once */
  Str.split class_sep_regex
};

let to_qual_string = String.concat sep::"::";

let module Match = {
  type quals_matcher = Str.regexp;
  let regexp_string_of_qualifiers quals => Str.quote (String.concat sep::"::" quals) ^ "$";
  let qualifiers_list_matcher quals_list =>
    (
      if (List.is_empty quals_list) {
        "a^" /* regexp that does not match anything */
      } else {
        List.map f::regexp_string_of_qualifiers quals_list |> String.concat sep::"\\|"
      }
    ) |> Str.regexp;
  let qualifiers_of_fuzzy_qual_name qual_name => {
    /* Fail if we detect templates in the fuzzy name. Template instantiations are not taken into
       account when fuzzy matching, and templates may produce wrong results when parsing qualified
       names. */
    if (String.contains qual_name '<') {
      failwithf "Unexpected template in fuzzy qualified name %s." qual_name
    };
    of_qual_string qual_name
  };
  let of_fuzzy_qual_names fuzzy_qual_names =>
    List.map fuzzy_qual_names f::qualifiers_of_fuzzy_qual_name |> qualifiers_list_matcher;
  let match_qualifiers matcher quals => {
    let normalized_qualifiers = {
      /* qual_name may have qualifiers with template parameters - drop them to whitelist all
         instantiations */
      let no_template_name s => List.hd_exn (String.split on::'<' s);
      List.map f::no_template_name quals
    };
    Str.string_match matcher (String.concat sep::"::" normalized_qualifiers) 0
  };
};
