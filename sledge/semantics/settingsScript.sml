(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Basic HOL initialisation *)

open HolKernel boolLib bossLib Parse;
local open wordsLib intLib stringLib integer_wordLib finite_mapLib alistLib
in end;

new_theory "settings";

(* Reduce HOL's ancient obsession with ALL_CAPS *)
overload_on ("map", ``MAP``);
overload_on ("sum", ``SUM : num list -> num``);
overload_on ("flookup", ``FLOOKUP``);
overload_on ("fempty", ``FEMPTY``);
overload_on ("fdom", ``FDOM``);
overload_on ("length", ``LENGTH``);
overload_on ("Some", ``SOME``);
overload_on ("None", ``NONE``);
overload_on ("snd", ``SND``);
overload_on ("fst", ``FST``);
overload_on ("zip", ``ZIP``);
overload_on ("el", ``EL``);
overload_on ("count_list", ``COUNT_LIST``);
overload_on ("Suc", ``SUC``);
overload_on ("flat", ``FLAT``);
overload_on ("all_distinct", ``ALL_DISTINCT``);
overload_on ("take", ``TAKE``);
overload_on ("drop", ``DROP``);
overload_on ("replicate", ``REPLICATE``);
overload_on ("every", ``EVERY``);
overload_on ("exists", ``EXISTS``);
overload_on ("list_rel", ``LIST_REL``);
overload_on ("reverse", ``REVERSE``);
overload_on ("log", ``LOG``);
overload_on ("option_map", ``OPTION_MAP``);
overload_on ("option_join", ``OPTION_JOIN``);
overload_on ("min", ``MIN``);
overload_on ("list_update", ``LUPDATE``);
overload_on ("last", ``LAST``);
overload_on ("fdiff", ``FDIFF``);
overload_on ("image", ``IMAGE``);
overload_on ("bigunion", ``BIGUNION``);
overload_on ("finite", ``FINITE``);
overload_on ("card", ``CARD``);
overload_on ("map_keys", ``MAP_KEYS``);
overload_on ("alookup", ``ALOOKUP``);
overload_on ("filter", ``FILTER``);
overload_on ("map2", ``list$MAP2``);
overload_on ("foldl", ``FOLDL``);
overload_on ("foldr", ``FOLDR``);
overload_on ("option_rel", ``OPTREL``);
overload_on ("front", ``FRONT``);
overload_on ("Inl", ``INL``);
overload_on ("Inr", ``INR``);
overload_on ("mem", ``MEM``);

export_theory ();
